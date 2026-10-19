# wip

Floating, always-on-top SDL3 window listing work items. Orange = in progress, green = done.
An item with `--color` uses that background; in the window a strip on its left edge keeps the status colour.
Click an item to open its URL; Cmd-click deletes it. Drag the header to move. `x` quits.
On macOS the window sits at NSStatusWindowLevel on every Space, including over full-screen apps.

Bindings: hand-written Haskell FFI bindings to SDL3 (`app/Sdl.hs`), loaded at runtime with
`dlopen` (`System.Posix.DynamicLinker`) and called through `foreign import ccall "dynamic"`
wrappers (`app/Ffi.hs`); Hackage has no SDL3 package.
Text uses SDL3_ttf with SF Mono 13pt (fallback Menlo; `--font PATH` overrides).
Without SDL3_ttf it falls back to the ASCII-only `SDL_RenderDebugText` font.
Only GHC boot packages are used; the HTTP server and client call libc sockets directly (`Wip.Socket`).

## Run

    brew install sdl3 sdl3_ttf hidapi jpeg-turbo
    cabal build all                   # GHC 9.14, cabal 3.16
    cabal run wip -- --port 7878      # --no-ui: no window; --no-deck: skip Stream Deck

## Stream Deck

Uses `../elgato-streamdeck` (Cabal package `elgato-streamdeck`, listed in `cabal.project`). The first connected deck
mirrors the list: one key per item, titled with the item name, orange or green;
an item colour fills the whole key.
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

## Session

Items are saved to `~/.local/state/wip/session-PORT.json` (`$XDG_STATE_HOME`
respected) shortly after each change and on exit, and restored on start.
`--state PATH` picks another file, `--no-state` disables it. The demo never saves.

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
`status` is `in_progress` or `done`. Items live in memory only.

## Test

    cabal test pkg:wip                # `cabal test wip` would name the executable

## Demo

    cabal run demo                    # 5 sample items, first one flips status every 3s (--flip 0 disables)
