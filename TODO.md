# TODO

## Ideas

### UI

- horizontal list of monitor boxes
  - name in border
  - in box small viewport for modes
- left, right to select monitor
- up, down to change mode of monitor
- ctrl left/right to move monitor
- ctrl up/down to align monitor with
  - min or max height?
  - top or bottom of left monitor?
- m? to toggle mirror
  - of left monitor?
  - of left most monitor
  - of left most monitor applied to all others
- d? to disable monitor?
  - or checkbox inside monitor box?

### Live update

- Listen to [socket2](https://wiki.hypr.land/IPC/) events and refresh on `monitoradded`, `monitorremoved` etc

## Resources

- [brick user guide](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst)
- [monitor configuration](https://wiki.hypr.land/Configuring/Monitors/)
- [IPC sockets](https://wiki.hypr.land/IPC/)
- [hyprctl syntax](https://wiki.hypr.land/Configuring/Using-hyprctl/)
