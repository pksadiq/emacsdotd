# -*- mode: snippet; require-final-newline: nil -*-
# name: <template>
# key: t.
# --
<template class="${1:AppWindow$(str-to-style yas-text "upcamel")}" parent="${2:GtkApplicationWindow$(str-to-style yas-text "upcamel")}">
$0
</template>