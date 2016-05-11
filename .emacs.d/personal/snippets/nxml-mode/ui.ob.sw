# -*- mode: snippet; require-final-newline: nil -*-
# name: <object> GtkScrolledWindow
# key: ob.sw
# --
<object class="GtkScrolledWindow" id="${1:scrolled$(replace-regexp-in-string " " "_" yas-text)}">
<property name="visible">${2:true}</property>
<property name="hscrollbar-policy">${3:never}</property>
<property name="vscrollbar-policy">${4:automatic}</property>${0:$$(read-only-mode)}
</object>