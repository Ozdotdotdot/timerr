# Changelog

## [v1.0.0] - 2025-11-18
### Added
- This is the first release of timerr, a Rust-based countdown timer CLI tool. Key features include:
   1. **Custom Messages**: Add a centered custom message under the timer using the -m or --message flag.
   2. **Colors**: Choose countdown colors via the --color flag, supporting named and hex formats.
   3. **Fonts**: Customize the timer display with three fonts: solid, hashy, and classic controlled by the -f or --font flag.
   4. **Smooth User Experience**: The timer includes advanced features like smooth color transitions (green → yellow → red), alternate screen buffers for clean rendering, and optional terminal bell behavior.