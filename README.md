<img src="./screenshot.png">

# What is it?

Tabbar.el is cool, but very ugly defaultly.

Every emacser will waste too much time to customize it.

So this extension provide an out of box configuration to use tabbar in Emacs.

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
| tabbar-switch-group                        | Switch tabbar group by ido fuzz match              |
| tabbar-select-beg-tab                      | Select first tab of current group                  |
| tabbar-select-end-tab                      | Select last tab of current group                   |
| tabbar-forward-tab-other-window            | Select next tab in other window                    |
| tabbar-backward-tab-other-window           | Select previous tab in other window                |
| tabbar-backward                            | Select the previous available tab                  |
| tabbar-forward                             | Select the next available tab                      |
| tabbar-backward-group                      | Go to selected tab in the previous available group |
| tabbar-forward-group                       | Go to selected tab in the next available group     |
| tabbar-kill-all-buffers-in-current-group   | Kill all buffers of current group                  |
| tabbar-kill-match-buffers-in-current-group | Kill buffers match extension of current group      |
| tabbar-keep-match-buffers-in-current-group | Keep buffers match extension of current group      |
| tabbar-move-current-tab-to-left     | Move current tab to left                           |
| tabbar-move-current-tab-to-right    | Move current tab to right                          |

If you're helm fans, you need add below code in your helm config:

```Elisp
(tabbar-build-helm-source)
```

Then add ```helm-source-tabbar-group``` in ```helm-source-list```

### Customize

| Option                  | Description                |
| :--------               | :----                      |
| tabbar-background-color | Background color of tabbar |
| tabbar-selected     | Active tab color           |
| tabbar-unselected   | Inactive tab color         |

