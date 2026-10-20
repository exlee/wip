#!/usr/bin/env ruby
# frozen_string_literal: true

# Mirrors the state of a Claude task into a running wip instance.
#
#   wip-status.rb start [--id ID] [--url URL] <title>   item in progress (orange), prints the id
#   wip-status.rb block <id>                            item red
#   wip-status.rb finish <id>                           item done (green)
#
# `start` re-adds the item, so running it again with the same id after
# `block` or `finish` puts the item back in progress.
# WIP overrides the binary (default `wip` on PATH), WIP_PORT the port.
# wip failures are reported on stderr; the exit status is always 0, so a
# missing wip never stops the task.

BLOCKED = "#E5484D"

def wip(*args)
  port = ENV["WIP_PORT"]
  cmd = [ENV.fetch("WIP", "wip"), *args]
  cmd.insert(2, "--port", port) if port && !port.empty?
  warn "wip-status: #{cmd.join(' ')}"
  out = IO.popen(cmd, err: %i[child out], &:read)
  warn "wip-status: wip failed: #{out.strip}" unless $?.success?
  $?.success? ? out.strip : nil
rescue SystemCallError => e
  warn "wip-status: #{e.message}"
  nil
end

def usage
  abort "usage: #{File.basename($PROGRAM_NAME)} start [--id ID] [--url URL] <title> | block <id> | finish <id>"
end

command, *rest = ARGV
case command
when "start"
  opts = []
  while (flag = rest.first) && %w[--id --url].include?(flag)
    usage if rest.size < 2
    opts << "#{flag}=#{rest[1]}"
    rest = rest.drop(2)
  end
  title = rest.join(" ").strip
  usage if title.empty?
  id = wip("add", *opts, title)
  puts id if id
when "block"
  usage unless rest.size == 1
  wip("color", rest.first, BLOCKED)
when "finish"
  usage unless rest.size == 1
  wip("color", rest.first, "none")
  wip("done", rest.first)
else
  usage
end
