# filechooser.el: an Emacs based xdg-desktop-portal filechooser. 

filechooser is an Emacs package that enables the use of Emacs to select files in applications like Firefox by providing an `xdg-desktop-portal` backend implementing the [filechooser](https://flatpak.github.io/xdg-desktop-portal/#gdbus-org.freedesktop.impl.portal.FileChooser) interface. It uses minibuffer to select a single file, so the experience is familiar one for commands like `find-file`. By default selecting multiple files uses a bespoke command which loops and completes one filename in minibuffer at a time. An alternative dired based selection is also available.

## Limitations
At this point `filechooser` has some serious limitations:
1. It requires a running Emacs instance with server also running. You can start the server by either running Emacs with the `--daemon` flag, or by typing `M-x server-start RET` in a non-daemon Emacs instance. If Emacs or server aren't running no file selection dialogues will appear and no error will be shown. More on this below.
2. If the systemd service `xdg-desktop-portal.service` starts before Emacs, it will be need to be restarted after the server has started to make filechooser work. A possible mitigation is to have

```emacs-lisp
(with-eval-after-load 'server
  (start-process "restart-portal" nil "systemctl" "--user" "restart" "xdg-desktop-portal.service"))
```
in your config file (This likely has a race condition).

3. I don't know a way of reliably positioning Emacs windows so cooperation from compositor/window manager is required. For example, I use sway and I have,
```
for_window [app_id="emacs" title="filechooser-frame"] {
    floating enable
}

for_window [app_id="emacs" title="filechooser-miniframe"] {
    floating enable
}
```
so that the frame filechooser uses appears above tiling windows.

4. The choices combo boxes are not implemented. I don't what would be a good interface for them (suggestions welcome) and in my (limited) experience I have not encountered them.

5. In my opinion minor compared to these is the fact the filechooser dialogues are not modal, i.e. you can still freely interact with the window that sent the file selection request.

6. Although `OpenFile` and `SaveFile` methods have been tested by me, `SaveFiles` method is completely untested since I don't know a way to trigger it.

## Installation
If despite these limitations you want to test the package you can use one of the following options:

### Manual Installation
1. Place `filechooser.el` file in the load path and generate the autoloads file for it.

2. Configure `xdg-desktop-portal` to use Emacs based filechooser. For recent versions this can be done by adding
```
org.freedesktop.impl.portal.FileChooser=emacs
```
to `~/.config/xdg-desktop-portal/portals.conf`. For older versions add your `$XDG_CURRENT_DESKTOP` to the `emacs.portal` file and copy it to `/usr/share/xdg-desktop-portal/portals/`.

3. Copy the `org.gnu.Emacs.FileChooser.service` file `/usr/share/dbus-1/services/`.

4. Restart `xdg-desktop-portal` by using `systemctl --user restart xdg-desktop-portal.service`.

### GNU ELPA
`filechooser.el` is available on GNU ELPA and can be installed using `M-x package-install`. This will take care of step 1 in the instructions for manual install but the other step will have to be taken manually.

### NixOS
Here's how to install this package on NixOS.

`configuration.nix:`
```
{ pkgs, ... }:

{
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;

    extraPortals = [
        # Add any fallback portals here
        pkgs.xdg-desktop-portal-gtk

        # Won't be needed with more recent xdg-desktop-portal versions
        (import ./emacs-portal.nix { inherit pkgs; })
      ];
      config.common = {
        default = [ "gtk" ];

        "org.freedesktop.impl.portal.FileChooser" = "emacs";
    };
  };
}
```

`emacs-portal.nix:`
```
{ pkgs }:
with pkgs;

stdenv.mkDerivation {
  pname = "Emacs portal";
  version = "git";

  src = fetchGit {
      name = "filechooser";
      url = "https://codeberg.org/rahguzar/filechooser";
      rev = "866304ab4244865108e12499f6e3be63e4981f92";
    };

  installPhase = ''
    mkdir -p $out/share/dbus-1/services/;
    cp org.gnu.Emacs.FileChooser.service $out/share/dbus-1/services/;
    mkdir -p $out/share/xdg-desktop-portal/portals/;
    cp emacs.portal $out/share/xdg-desktop-portal/portals/;
    '';
}
```

## Usage
Start an application that uses `xdg-desktop-portal`. An example which is my main use case is to launch `GTK_USE_PORTAL=1 firefox` and initiate a file selection dialogue. Unless you have changed the value of `filechooser-use-popup-frame, a minibuffer only Emacs frame will hopefully appear at this point. You can  select the file (or files depending on the request) by the same means you do usually in minibuffer.

The only new feature is that you can use different filters to determine which files are presented for completion. Some of these filters are defined by the request, but you can also define your own in `filechooser-filters`. To toggle a filter use `C-f`, this will present all filters grouped according to whether they are active or inactive. Select one to toggle it. Only those files which pass through an active filter are presented for completion.

If you are selecting multiple files `M-TAB` toggles selection of the current file without exiting the file selection dialogue. `RET` exits while toggling the selection of current file. If you want to exit with currently selected files without toggling the currently highlighted candidate use `M-RET`. `M-a` selects all of current candidates.

## Customization
### Popup frames
By default, `filechooser` uses a new frame for presenting the file selection dialogue. This can be disabled by setting `filechooser-use-popup-frame` to `nil`.
### File selection interface
By default `completing-read` based interfaces are used for all operations. These interfaces are governed by the value of `filechooser-choose-file`, `filechooser-choose-files` and `filechooser-choose-directory`. The value of each of the these variables is a function that is called to do completion, see their doc strings for what is expected from these functions.

The only builtin alternative to default values is `filechooser-with-dired`. You can use,
```emacs-lisp
(setq filechooser-choose-files #'filechooser-with-dired)
```
to change to a dired based selection for multiple files. In this mode the file chooser frame will have a dired buffer selected. To select files navigate to the desired directory. Mark the files to be selected and press `C-c C-c` to finish. `C-c C-k` aborts the selection process. Filters are available in this mode too and can be changed using `C-f`.

### Keybinds
If you want to use some commands during file selection you can them on `filechooser-mininuffer-map`. For binding a command only during multiple file selection use `filechooser-multiple-selection-map`.

## Integration with other packages
### Vertico
If you [vertico](https://github.com/minad/vertico) as your completion UI, the following might be more intuitive for selecting multiple files,
```emacs-lisp
(with-eval-after-load 'filechooser
  (defun +filechooser-multiple-vertico-tab ()
    (interactive)
    (vertico-insert)
    (unless (file-directory-p (minibuffer-contents))
      (filechooser-multiple-continue)))
  (define-key filechooser-multiple-selection-map
              (kbd "TAB") #'+filechooser-multiple-vertico-tab))
```
This rebinds `TAB` so what if you press it with a directory highlighted, it enters the directory, while if you press with a file highlighted, the selection of that file is toggled and you can continue selecting more files.
## Possible Improvements
If you have some suggestions for improvement, please file a bug report or even better a start a pull request. Especially welcome are implementations for:
1. [Choices](https://flatpak.github.io/xdg-desktop-portal/docs/doc-org.freedesktop.portal.FileChooser.html#org-freedesktop-portal-filechooser-openfile) combo boxes.
2. An auto start mechanism that starts an emacsclient and use it if Emacs or server are not running.
## More on limitations
Originally I wanted to write a portal which acted as a global minor mode i.e a user would run `M-x filechooser-mode` and Emacs would take over file selection dialogues. User would run `M-x filechooser-mode` again and Emacs would no longer meddle with these dialogues. However this seems not be possible. [xdg-desktop-portal does not allow user specific portal definitions](https://github.com/flatpak/xdg-desktop-portal/issues/804), as a result to place the `emacs.portal` file in the correct location root access is required. Once that file is placed and `xdg-desktop-portal` restarted, only Emacs can be used for `filechooser` interface. If Emacs doesn't claim the `dbus` service required, no file selection dialogues will appear and no error will be raised either (at least `firefox` fails completely silently) since there is no fallback mechanism to use a working service if the first one couldn't be started.
