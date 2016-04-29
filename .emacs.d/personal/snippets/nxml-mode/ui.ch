# -*- mode: snippet; require-final-newline: nil -*-
# name: <child>
# key: ch.
# --
<child${1: type="${2:titlebar}"}>
<object class="${3:GtkButton$(str-to-style yas-text "upcamel")}" id="$4">
<property name="visible">${5:True}</property>
<property name="can-focus">${6:False}</property>$0
</object>
</child>