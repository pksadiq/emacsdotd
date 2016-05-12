# -*- mode: snippet; require-final-newline: nil -*-
# name: <style>
# key: st.
# --
<style>
<class name="${1:image-button$(replace-regexp-in-string " " "-" yas-text)}"/>${0:$$(read-only-mode)}
</style>