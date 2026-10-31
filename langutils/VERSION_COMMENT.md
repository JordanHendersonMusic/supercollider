# Version Comments

To enable new language features without breaking backwards compatibility, we have a new feature, version comments.

```
//# VERSION legacy
//# VERSION 3.16
```

These can only be placed in class library files. 
They either go at the top of the file, in which case, they apply to all classes in the file, or as the top line inside a class's body, in which case they set the version *just* for that one class.

Interpreted file cannot have their own language version because it would be unclear if this refered to a region, or a file. 
Instead this is set with a language config flag or `sclang` command line flag and applies to every scd file.
