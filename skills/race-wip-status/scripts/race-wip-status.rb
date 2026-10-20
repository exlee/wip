#!/usr/bin/env ruby
# frozen_string_literal: true

# Marks the caller's RACE terminal with the state of the current task and
# mirrors the state into a running wip instance.
#
#   race-wip-status.rb start "<title>"   RACE: title, orange accent, important on, blink off
#                                        wip: item in progress, linked to the terminal
#   race-wip-status.rb block             RACE: red accent, border blinks until clicked
#                                        wip: item red
#   race-wip-status.rb finish            RACE: green accent, blink off
#                                        wip: item done
#
# The wip item id is the terminal number and its URL is the terminal's
# race:// link, so a click on the item (window or Stream Deck key) focuses
# the terminal.
#
# RACE_CONTROL overrides the race-control binary, RACE_TERMINAL the target
# (`terminal:12`, `pid:1234`; default `caller`). WIP overrides the wip
# binary, WIP_PORT its port. wip failures are ignored.

require "json"

ACCENTS = {
  "start" => "#F76C14",
  "block" => "#E5484D",
  "finish" => "#99D52A"
}.freeze

APP_CONTROL = "/Applications/RACE.app/Contents/MacOS/race-control"
TARGET = ENV.fetch("RACE_TERMINAL", "caller")

def race_control
  ENV["RACE_CONTROL"] ||
    ENV.fetch("PATH", "").split(File::PATH_SEPARATOR).map { |d| File.join(d, "race-control") }
       .find { |p| File.executable?(p) } ||
    APP_CONTROL
end

RACE_CONTROL = race_control

def race(*args)
  puts "race-control #{args.join(' ')}"
  ok = system(RACE_CONTROL, *args)
  abort "race-control failed: #{args.first} (exit #{$?.exitstatus})" unless ok
end

# Terminal number and race:// link of TARGET, or nil when RACE cannot tell.
def terminal_info
  if (number = TARGET[/\A(?:terminal:)?(\d+)\z/, 1])
    return [number, "race://terminal/#{number}"]
  end

  pid = TARGET.start_with?("pid:") ? TARGET.delete_prefix("pid:") : Process.pid.to_s
  out = IO.popen([RACE_CONTROL, "--json", "find-pid", pid], err: File::NULL, &:read)
  element = JSON.parse(out).fetch("elements", []).first
  return nil unless element

  [element["id"].delete_prefix("terminal:"), element["link"]]
rescue StandardError
  nil
end

def wip(*args)
  port = ENV["WIP_PORT"]
  cmd = [ENV.fetch("WIP", "wip"), *args]
  cmd.insert(2, "--port", port) if port && !port.empty?
  puts cmd.join(" ")
  system(*cmd, out: File::NULL, err: File::NULL)
end

def usage
  abort "usage: #{File.basename($PROGRAM_NAME)} start <title> | block | finish"
end

abort "not found: #{RACE_CONTROL}" unless File.executable?(RACE_CONTROL)

command, *rest = ARGV
case command
when "start"
  title = rest.join(" ").strip
  usage if title.empty?
  race("rename", TARGET, title)
  race("accent", TARGET, ACCENTS["start"])
  race("important", TARGET, "on")
  race("blink", TARGET, "off")
  id, link = terminal_info
  wip("add", "--id=#{id}", "--url=#{link}", title) if id
when "block"
  race("accent", TARGET, ACCENTS["block"])
  race("blink", TARGET, "on")
  id, = terminal_info
  wip("color", id, ACCENTS["block"]) if id
when "finish"
  race("accent", TARGET, ACCENTS["finish"])
  race("blink", TARGET, "off")
  id, = terminal_info
  if id
    wip("color", id, "none")
    wip("done", id)
  end
else
  usage
end

puts "#{command}: done"
