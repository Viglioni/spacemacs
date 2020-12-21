# repo

Clone this repo in your ~/

# installs
third party installs

`brew install borkdude/brew/clj-kondo`

`brew install ispell`

`brew install aspell`

`npm i -g tern prettier`

`pip3 install pygments`

`npm install -g import-js`

`npm install -g tsun`

# .private

## slack-config.el

example

```lisp

(slack-register-team
 :name "firma"
 :default t
 :client-id "laura@firma.com"
 :client-secret "h4cK3r"
 :token "j r r token"
 :subscribed-channels '(general))

```

# spacemacs script

Call spacemacs anywhere the same way you call emacs e.g. `$ spacemacs App.clj`

```sh
$ export PATH="~/spacemacs/scripts:$PATH"
```

# Manual fixes or changes directly in `.emacs.d/`

1. Added gif to homepage, changed buffer name etc: [commit](https://github.com/Viglioni/spacemacs/commit/f1e14a15193bb6966325944fb5cdc84113425c65)
1. Linum bug: [spacemacs commit](https://github.com/syl20bnr/spacemacs/pull/9209/files ) : [my commit](https://github.com/Viglioni/spacemacs/commit/85b437e16a9c76157a8a9b4f82de9de570318231 ) 
