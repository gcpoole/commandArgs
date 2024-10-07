## Package commandArgs

This package has a single function to convert the results from `commandArgs()` in R to a more useful format, with basic error checking.

If we consider the following simple R script:
```
print(
  commandArgs()
)
```
saved in a file called `echo_command_args.R`, we can run the script from the command line like this:

`Rscript echo_command_args.R --foo=bar --foo2`

And we will see results like this:

```
[1] "/usr/lib/R/bin/exec/R"      "--no-echo"                 
[3] "--no-restore"               "--file=echo_command_args.R"
[5] "--args"                     "--foo=bar"                 
[7] "--foo2"                    
```

Note that the first 5 value are added automatically by Rscript.  By using the `commandArgs` package, the above can be easilty converted to:

```
  foo   foo2 
 "bar" "TRUE"
```
which is the equivalent of 

`c(foo = "bar", foo2 = "TRUE")`

There is some additional error checking to be sure that: 

- any required parameters are present
- any parameters that require arguments have arguments
- all parameters are recognized.
