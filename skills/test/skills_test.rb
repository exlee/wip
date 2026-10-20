# frozen_string_literal: true

# Runs both skill scripts against fake `wip` and `race-control` binaries that
# log their arguments.  ruby skills/test/skills_test.rb

require "minitest/autorun"
require "fileutils"
require "open3"
require "tmpdir"

SKILLS = File.expand_path("..", __dir__)
WIP_STATUS = File.join(SKILLS, "wip-status/scripts/wip-status.rb")
RACE_WIP_STATUS = File.join(SKILLS, "race-wip-status/scripts/race-wip-status.rb")

class SkillScriptsTest < Minitest::Test
  def setup
    @dir = Dir.mktmpdir
    @log = File.join(@dir, "calls.log")
    @wip = fake("wip", 'echo "${WIP_FAKE_ID:-7}"')
    @race = fake("race-control", "")
  end

  def teardown
    FileUtils.rm_rf(@dir)
  end

  def fake(name, body)
    path = File.join(@dir, name)
    File.write(path, "#!/bin/sh\necho \"#{name} $*\" >> '#{@log}'\n#{body}\n")
    File.chmod(0o755, path)
    path
  end

  def run_script(script, *args, env: {})
    env = { "WIP" => @wip, "RACE_CONTROL" => @race, "WIP_PORT" => nil }.merge(env)
    out, err, status = Open3.capture3(env, "ruby", script, *args)
    [out, err, status]
  end

  def calls
    File.exist?(@log) ? File.readlines(@log, chomp: true) : []
  end

  def test_start_adds_item_and_prints_id
    out, _, status = run_script(WIP_STATUS, "start", "--id", "pr-42", "--url", "https://x.test/42", "Review", "PR")
    assert status.success?
    assert_equal "7", out.strip
    assert_equal ["wip add --id=pr-42 --url=https://x.test/42 Review PR"], calls
  end

  def test_port_goes_after_the_subcommand
    run_script(WIP_STATUS, "block", "3", env: { "WIP_PORT" => "7900" })
    assert_equal ["wip color --port 7900 3 #E5484D"], calls
  end

  def test_finish_clears_color_then_marks_done
    run_script(WIP_STATUS, "finish", "3")
    assert_equal ["wip color 3 none", "wip done 3"], calls
  end

  def test_missing_wip_does_not_fail
    out, _, status = run_script(WIP_STATUS, "start", "Task", env: { "WIP" => File.join(@dir, "absent") })
    assert status.success?
    assert_empty out
  end

  def test_start_without_title_is_rejected
    _, _, status = run_script(WIP_STATUS, "start", "--id", "x")
    refute status.success?
    assert_empty calls
  end

  def test_race_start_links_item_to_terminal
    run_script(RACE_WIP_STATUS, "start", "Fix", "build", env: { "RACE_TERMINAL" => "terminal:12" })
    assert_includes calls, "race-control rename terminal:12 Fix build"
    assert_equal "wip add --id=12 --url=race://terminal/12 Fix build", calls.last
  end

  def test_race_block_and_finish
    env = { "RACE_TERMINAL" => "12" }
    run_script(RACE_WIP_STATUS, "block", env: env)
    run_script(RACE_WIP_STATUS, "finish", env: env)
    wip_calls = calls.grep(/\Awip /)
    assert_equal ["wip color 12 #E5484D", "wip color 12 none", "wip done 12"], wip_calls
    assert_includes calls, "race-control blink 12 on"
    assert_includes calls, "race-control blink 12 off"
  end

  def test_race_failure_stops_before_wip
    fake("race-control", "exit 1")
    _, _, status = run_script(RACE_WIP_STATUS, "start", "Task", env: { "RACE_TERMINAL" => "12" })
    refute status.success?
    assert_empty calls.grep(/\Awip /)
  end
end
