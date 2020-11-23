# emacs-password-genarator

## Generate passwords inside Emacs

This package provides simple functions to create passwords and insert them inside buffer immediately. I've tried to cover most common cases of passwords.

## Simple usage:

`M-x password-generator-simple` - generate 8 characters password valid for almost all websites. Like: `J9l1Ekw2`

`M-x password-generator-strong` - generate 12 characters password with some special characters. THis type of password is valid for most of websites, and also will not hurt you if you will pass it to via shell script or so. Like: `DSSAxnF@SyL,`

`M-x password-generator-paranoid` - generate 20 characters password for paranoids, who does not afraid of those create-password-with-specaial-characters-no-dumbass-not-that-special-characters! sites. Like: `y2%c74r>_2cLgGC,$Q)_`

`M-x password-generator-phonetic` - generate 8 characters password, which most of us can simply remember. Like: `pa4zi3go` or `za3hi2de` or `ke5ho7pe`

`M-x password-generator-numeric` - generate 4 digits password.

`M-x password-generator-words` - generate password from 4 random of most used 1500 english words. "." is default separator.

`M-x password-generator-custom` - generate password your alphabet.


## Changing password length:

`C-u 10 M-x password-generator-numeric` - generate 10 digits password.
`C-u 32 M-x password-generator-paranoid` - generate 32 characters password with specil symbols.

etc.

`C-u 5 M-x password-generator-words` - generate password from 5 random words, but exact length is unpredicatble!


## Customizing the package:

See `M-x customize-group` for group password-generator .


## Generate password but not to insert it:

`(password-generator-strong 10 t)` - will return 10 characters length password.


## Generate passwords from your own alphabet:

`password-generator-custom` is used for generating passwords which is phone friendly, or containing symbols from your national language. To define its behavior customize variables `password-generator-custom-length` and `password-generator-custom-alphabet`.


### Please star the repo if you find this package useful!

Thank you for using this software.
