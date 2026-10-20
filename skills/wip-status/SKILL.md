---
name: wip-status
description: Show the state of the current task as an item in the wip window and on the Stream Deck. Run at the start of every task (item in progress, orange), when blocked waiting on the user (red), and when done (green). Use when wip is running and RACE is not used.
allowed-tools: bash
---

# wip task status

One script, three states. Paths are relative to this skill directory.

| Moment | Command | Item |
|---|---|---|
| Task starts | `scripts/wip-status.rb start [--id ID] [--url URL] "<title>"` | added in progress (orange); prints the id |
| Blocked on the user | `scripts/wip-status.rb block <id>` | red |
| Task finished | `scripts/wip-status.rb finish <id>` | done (green) |

## Rules

- Run `start` before the first action of every new task, including follow-ups.
- Keep the title short: the work, not the project.
- Remember the id that `start` prints. Pass the id to `block` and `finish`.
- Pass `--id` with the remembered id when the same task starts again, so the
  existing item is reused instead of a new one.
- Pass `--url` when the task has a natural link (pull request, issue, ticket).
  A click on the item opens the link.
- Run `block` before asking the user a question you cannot proceed without.
- Run `start` again with the same `--id` and title when work resumes after a block.
- Run `finish` as the last action of the task.
- When `start` prints no id, wip is not running. Skip later calls silently.

## Notes

- `WIP` overrides the wip binary (default `wip` on PATH), `WIP_PORT` the port
  (default 7878).
- The script always exits 0; wip errors go to stderr.
