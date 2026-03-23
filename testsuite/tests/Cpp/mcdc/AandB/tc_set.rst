**Exercise mcdc on a "a && b" expression instated in a variety of contexts**

--

The code to test always features a "tryme" entry point expecting
the Boolean values to use for "a" and "b", as well as a "skip" argument
to request an early return.

Expectation anchors:

/eval/  Line where the a && b expression is evaluated.
/true/  Regular code executed when the expr evaluates true.
/false/ Regular code executed when the expr evaluates false.
/other/ Regular code executed regardless of the expr value.

/lambda_true/   Code executed when the expr evaluates true
                as part of a lambda function.
/lambda_false/  Code executed when the expr evaluates false
                as part of a lambda function.
/lambda_other/  Other lines constituting a lambda expression,
                expected not to contain any stmt of their own.

/test_skip/ Line testing the "skip" argument.
/skip/      Code executed when "skip" is true.
