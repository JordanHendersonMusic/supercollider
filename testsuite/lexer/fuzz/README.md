## To run the fuzz

# 1 

Use clang to compile the lexer_fuzz_with_old target.


# 2

Run 
```
mkdir corpus
./lexer_fuzz_with_old corpus -dict=dict -only_ascii=1 -max_len=15  
```

Optionally add the `jobs=N` flags to run in parallel.

This will run forever adding to the corpus. If it finds a difference between the old and new, it will crash, generating a (somewhat) readable error message.
