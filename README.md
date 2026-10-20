# wip

A floating, always-on-top list of work in progress, mirrored onto an Elgato
Stream Deck. Agents and scripts add items through a CLI or an MCP server; each
item shows whether the work is in progress, blocked or done.
A click on an item (or a press of its Stream Deck key) opens the item link.

![wip window with items in progress, blocked, custom-coloured and done](docs/window.png)

- Orange: in progress. Green: done. Any other colour: set by `--color` or `wip color`
  (for example red for "blocked on me").
- Click an item to open its URL. Cmd-click deletes it (macOS).
  Drag a row to reorder; drag the header to move the window. `x` quits.
- On macOS the window sits at NSStatusWindowLevel on every Space, including over
  full-screen apps, and never takes focus.

## Stream Deck

wip changes what the Stream Deck shows and reacts to its keys. It integrates
with Stream Deck devices through [elgato-streamdeck](https://github.com/exlee/haskell-elgato-streamdeck),
a Haskell port of the Rust crate. Tested with the Stream Deck Neo.

The first connected deck mirrors the list: one key per item, titled with the item
name, orange or green; an item colour fills the whole key.
Keys are ordered by latest activity (add or status change), newest first; any new
activity jumps back to page 1.

- Short press opens the URL (if any). Hold 0.6s to delete the item.
- Neo: the left/right touch buttons page back/forward. The info screen shows
  `Working X / Done Y / Total Z` and the page.
- Decks without touch buttons use the last key as a cycling "next page" key
  when items do not fit.
- Titles shrink to fit without splitting words; below ~11% of the key height,
  words too long are hyphenated at syllables.
- Keys (and the Neo screen) are blanked on exit; replugging is handled.

## RACE

With [RACE](https://race-term.com), each Claude session gets one wip item linked
to its terminal through a `race://terminal/N` deep link. A click on the item,
in the window or on the Stream Deck, focuses that terminal in RACE immediately.
The `race-wip-status` skill below sets this up.

![wip window tracking Claude sessions in RACE terminals](docs/race-session.png)

## Install

Binaries for Linux and macOS are attached to each
[GitHub release](https://github.com/exlee/wip/releases).
SDL3, SDL3_ttf, hidapi and turbojpeg are loaded at runtime:

    brew install sdl3 sdl3_ttf hidapi jpeg-turbo    # macOS

On Linux, install the SDL3, SDL3_ttf, hidapi and libturbojpeg packages of your distribution.

On Linux, also copy [`40-streamdeck.rules`](https://github.com/exlee/haskell-elgato-streamdeck/blob/main/40-streamdeck.rules)
to `/etc/udev/rules.d/` and replug the deck.

Without SDL3_ttf, text falls back to the ASCII-only `SDL_RenderDebugText` font.
Without SDL3, run with `--no-ui`. Without hidapi, the deck is skipped.

## Build

    cabal build all                   # GHC 9.14, cabal 3.16
    cabal run wip -- --port 7878      # --no-ui: no window; --no-deck: skip Stream Deck

`cabal.project` fetches elgato-streamdeck from GitHub. To work on both, put
`packages: ../elgato-streamdeck` in `cabal.project.local`.

Bindings: direct Haskell FFI bindings to SDL3 (`app/Sdl.hs`), loaded at runtime with
`dlopen` (`System.Posix.DynamicLinker`) and called through `foreign import ccall "dynamic"`
wrappers (`app/Ffi.hs`); Hackage has no SDL3 package.
Text uses SDL3_ttf with SF Mono 13pt (fallback Menlo, then DejaVu Sans Mono; `--font PATH` overrides).
Only GHC boot packages are used; the HTTP server and client call libc sockets directly (`Wip.Socket`).

## CLI

Talks to a running instance (default port 7878, `--port N` to change):

    wip add --url=https://example.com Fix the build   # prints the new id; --url is optional
    wip add --id=pr-42 Review PR 42                   # custom id; re-adding it updates the item
    wip add --color=steelblue Deploy                  # background: CSS colour name or hex (#f80, #ff8800)
    wip done pr-42
    wip color pr-42 '#f80'                            # name, hex, or none to clear
    wip done 3
    wip delete 3
    wip list                                          # id, status, title, url (tab-separated)

## MCP

Streamable HTTP endpoint: `http://127.0.0.1:7878/mcp`

    claude mcp add --transport http wip http://127.0.0.1:7878/mcp

Tools: `list_items`, `add_item(title, url?, status?, id?, color?)`, `delete_item(id)`, `set_status(id, status)`, `set_color(id, color)`.
`status` is `in_progress` or `done`.

## Claude skills

`skills/` holds two Claude Code skills that keep one wip item per task up to date:
orange when work starts, red when Claude is blocked on you, green when done.

- `wip-status`: wip only. The item can carry a link (pull request, ticket).
- `race-wip-status`: also renames and colours the RACE terminal, and links the
  item to the terminal.

Install one by linking it into your skills directory:

    ln -s "$PWD/skills/race-wip-status" ~/.claude/skills/race-wip-status

Then tell Claude, for example in `CLAUDE.md`: "Use the race-wip-status skill
at the start of every task, when blocked, and when done."
The scripts need Ruby and `wip` on PATH (`WIP` overrides the binary).

## Session

Items are saved to `~/.local/state/wip/session-PORT.json` (`$XDG_STATE_HOME`
respected) shortly after each change and on exit, and restored on start.
`--state PATH` picks another file, `--no-state` disables it. The demo never saves.

## Test

    cabal test pkg:wip                # `cabal test wip` would name the executable
    ruby skills/test/skills_test.rb

## Demo

    cabal run demo                    # 5 sample items, first one flips status every 3s (--flip 0 disables)

## License

MPL-2.0, see `LICENSE`.
