** Exercise SC on package with deferred constant definition**

Check that neither the deferred constant declaration (without the intializer)
nor the definition (with the initializer) is reported uncovered when the
package is included in the elaboration closure.

Check that statements using the deferred constant value are treated as other
statements, and that code reached as a side effect of the constant
initialization (e.g. if this initialization involves a function call) is
marked as code reached by other means.

LRMREF: 7.4
