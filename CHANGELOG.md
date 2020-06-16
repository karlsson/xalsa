## 0.3.0 - 2020-06-16

### Added
- AVX for X86_64 machines
- Configurable ratio between buffer and period size
- Configurable period size
- Direct conversion (hw vs. plughw)
- Nerves build for RPi
- Set SCHED_RR realtime policy if possible

### Changed
- Use enif_select instead of callback
- Use mmap instead of copy write
- Number of channels configured as key-value now (see config/config.exs)
