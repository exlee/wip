---
name: race-wip-status
description: Mark the current RACE terminal with task state and mirror the state into wip, with a link back to the terminal. Run at the start of every task (title, orange accent, wip item in progress), when blocked waiting on the user (red), and when done (green, wip item done). Use when Claude runs inside a RACE terminal.
allowed-tools: bash
---

# RACE + wip task status

One script, three states. It targets the RACE terminal it runs in (`caller`).
Paths are relative to this skill directory.

| Moment | Command | RACE terminal | wip item |
|---|---|---|---|
| Task starts | `scripts/race-wip-status.rb start "<title>"` | title set, accent `#F76C14`, important on, blink off | in progress (orange) |
| Blocked on the user | `scripts/race-wip-status.rb block` | accent `#E5484D`, border blinks until clicked | red |
| Task finished | `scripts/race-wip-status.rb finish` | accent `#99D52A`, blink off | done (green) |

The wip item id is the terminal number. The item URL is the terminal's
`race://terminal/N` deep link: a click on the item in the wip window, or a
press of its Stream Deck key, focuses the terminal in RACE.

## Rules

- Run `start` before the first action of every new task, including follow-ups.
- Keep the title short: the work, not the project.
- Run `block` before asking the user a question you cannot proceed without.
- Run `start` again with the same title when work resumes after a block.
- Run `finish` as the last action of the task.

## Notes

- Uses `race-control` (on PATH, else inside `/Applications/RACE.app`), which
  drives the same socket as the RACE MCP server. `RACE_CONTROL` overrides the
  binary, `RACE_TERMINAL` the target (`terminal:12`, `pid:1234`).
- `WIP` overrides the wip binary, `WIP_PORT` its port.
- Exit status is non-zero when RACE refuses or is not running. wip errors are
  ignored.
