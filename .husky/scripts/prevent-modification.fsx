printfn
    """Changes to:

- web/QuickTest/QuickTest.fs

should not be committed.

This is to avoid polluting history with prototyping code.

If you really need to commit these changes, you can bypass this check by using:

    git commit --no-verify

or a corresponding setting in your Git client.
"""

System.Environment.Exit 1
