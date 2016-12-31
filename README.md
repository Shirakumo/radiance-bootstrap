## About Radiance-Bootstrap
This is a system that offers a single file to bootstrap Radiance deployed installations from. Using ASDF the system can be concatenated down to a single file that can be `load`ed in. The file will then handle the installation and configuration of a basic setup.

## Using It
Download the file and load it in.

```bash
curl -O https://raw.githubusercontent.com/Shirakumo/radiance-bootstrap/master/bin/radiance-bootstrap.lisp
sbcl --script radiance-bootstrap.lisp
```

It'll ask you some questions about the installation interactively. That's it.

## Compiling the Bootstrapper
If you want to generate the full bootstrap file yourself, you can do so like this:

```commonlisp
(asdf:operate 'asdf:build-op :radiance-bootstrap)
```
