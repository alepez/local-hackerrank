# local-hackerrank

An utility to test programs against a text input and an expected output.

I use it to test locally my solutions to hackerrank.

## Example

This command will compile `bon-appetit.hs` and run it with `input.txt` as
standard input and the output will be checked against `output.txt`.

```
local-hackerrank-exe --lang hs --program bon-appetit.hs --input input.txt --output output.txt
```
