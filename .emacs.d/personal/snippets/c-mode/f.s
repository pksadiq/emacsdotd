# -*- mode: snippet -*-
# name: function for signals
# key: f.s
# --
${1:static }${2:void}
${3:`(or gnome-sig-defun "my_function")`} (${4:GApplication *app},
      gpointer user_data)
{
  $0
}`(unless (c-next-line-empty-p) "\n")`