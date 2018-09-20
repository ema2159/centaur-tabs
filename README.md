<img src="./screenshot.png">

# What is it?

Provide an out of box configuration to use tab in Emacs.

### Installation
You need install [projectile](https://github.com/bbatsov/projectile) first.

Then put awesome-tab.el to your load-path.

The load-path is usually ~/elisp/.

It's set in your ~/.emacs like this:

```Elisp
(add-to-list 'load-path (expand-file-name "~/elisp"))
(require 'awesome-tab)
```

### Usage.

| Command                                    | Description                                        |
| :--------                                  | :----                                              |
| awesome-tab-switch-group                        | Switch awesome-tab group by ido fuzz match              |
| awesome-tab-select-beg-tab                      | Select first tab of current group                  |
| awesome-tab-select-end-tab                      | Select last tab of current group                   |
| awesome-tab-forward-tab-other-window            | Select next tab in other window                    |
| awesome-tab-backward-tab-other-window           | Select previous tab in other window                |
| awesome-tab-backward                            | Select the previous available tab                  |
| awesome-tab-forward                             | Select the next available tab                      |
| awesome-tab-backward-group                      | Go to selected tab in the previous available group |
| awesome-tab-forward-group                       | Go to selected tab in the next available group     |
| awesome-tab-kill-all-buffers-in-current-group   | Kill all buffers of current group                  |
| awesome-tab-kill-match-buffers-in-current-group | Kill buffers match extension of current group      |
| awesome-tab-keep-match-buffers-in-current-group | Keep buffers match extension of current group      |
| awesome-tab-move-current-tab-to-left     | Move current tab to left                           |
| awesome-tab-move-current-tab-to-right    | Move current tab to right                          |

If you're helm fans, you need add below code in your helm config:

```Elisp
(awesome-tab-build-helm-source)
```

Then add ```helm-source-awesome-tab-group``` in ```helm-source-list```

### Customize

| Option                  | Description                |
| :--------               | :----                      |
| awesome-tab-background-color | Background color of awesome-tab |
| awesome-tab-selected     | Active tab color           |
| awesome-tab-unselected   | Inactive tab color         |
