[33mcommit 71a00e01fa50c5184be5e799d992bdd5e5361900[m[33m ([m[1;36mHEAD -> [m[1;32mconflict_file[m[33m)[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Mon Jan 15 09:04:43 2018 -0700

    Generator now prints a warning if there are unused conflict resolutions specified in the Grammer file.

[33mcommit 9a078076c49e49e739527f54105f9daa4b46cf6f[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Sun Jan 14 18:53:16 2018 -0700

    Extended Reader to be able to handle grammers with conflict resolutions.
    The generator now looks to the resolutions found in the grammer file to
    resolve conflicts.

[33mcommit c7f4c7814d71cf0674b975e5bdb23ca23da4eba5[m[33m ([m[1;32minternal_resolution[m[33m)[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Sun Jan 14 18:34:02 2018 -0700

    The generator now looks to the production classes for resolutions for
    conflict. If resolutions haven't been specified in the classes, the
    generator reports an error.

[33mcommit 10e02b45974f352249b4c04ba4b824fa4ce870c6[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Sun Jan 14 15:05:40 2018 -0700

    Added a reader class which reads a grammer from a file, which
    productions written in the form ":Terminal Name > Symbol_1 Symbol_2
    Symbol_3".

[33mcommit 4b476e4082cc1915a872b7de0b66dca7e1686146[m[33m ([m[1;31morigin/master[m[33m, [m[1;32mmaster[m[33m)[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Sat Jan 13 21:56:26 2018 -0700

    Split up the output file between source and header so that source can directly overwrite the Parser file in the compiler project.

[33mcommit 792b420a670254d882e3edbc7a6f4ba020f77d28[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Thu Jan 11 19:35:44 2018 -0700

    Generated code for parser now has descriptive names for all the derived symbol classes.

[33mcommit 7ee9f2c78390a978f00149f40ee7874fe71399f0[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Mon Jan 8 16:06:13 2018 -0700

    The generator now prints out the entered inputs into a file, allowing the inputs to be copy pasted in when needed to regenerate the grammer.

[33mcommit 2e50f7121989fc4a67b9bc692d7d30452e28e5e5[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Mon Jan 1 23:14:55 2018 -0700

    The generator now takes 3 file inputs from the lexer generator and produces a fully integrated parser with lexer. Fixed multiple bugs in the Parser code produced.

[33mcommit bed1a52ba766943e7d9047e45b5553bbcd7cb998[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Wed Dec 27 17:52:03 2017 -0700

    Can now select resolutions to conflicts when they arise. Fixed minor standard violations detected by g++.

[33mcommit e0b82b543a3029d67c941d36e0a79d0b6aaadebf[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Tue Dec 26 23:21:18 2017 -0700

    Conflict descriptions are now printed.

[33mcommit 4c309148e3c8fc7169020e3061759ec2c83a5d6c[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Tue Dec 26 21:49:06 2017 -0700

    Added comments to a large portion of the code to make it easier to understand and modify. The rest will be commented in a future commit.

[33mcommit f6325da84853c4628083e82bb60b51122ccc4375[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Thu Dec 21 17:14:38 2017 -0700

    Minor syntax error fixes in the output parser code.

[33mcommit 24ed762efb84774c54f81aad32fea9cc500bb56f[m
Author: martin-bobek <martinbobek77@hotmail.com>
Date:   Wed Dec 20 21:38:21 2017 -0700

    Who the fuck knows. Looks like lots of reordering, probably refactoring, and maybe more...

[33mcommit 0f7d916a445c384f4bd2873836147f6b4d8570b2[m
Author: Martin Bobek <martinbobek77@hotmail.com>
Date:   Thu May 18 02:31:51 2017 -0600

    Now gets input from User. No validation is performed.

[33mcommit 4289d8cf231c222a54096d0e87531214513a8519[m
Author: Martin Bobek <martinbobek77@hotmail.com>
Date:   Thu May 18 00:00:55 2017 -0600

    Generates working parser.

[33mcommit 61903b11025cd7b4ae391d25567d80575d1e4c62[m
Author: Martin Bobek <martinbobek77@hotmail.com>
Date:   Tue May 9 14:44:17 2017 -0600

    Fixed major bugs.

[33mcommit f2793143017971dafd41b1f93d2a21b82ab9a4de[m
Author: Martin Bobek <martinbobek77@hotmail.com>
Date:   Tue May 9 01:11:00 2017 -0600

    Untested Nfa generator.

[33mcommit 2e387cf6faeb71a0a036a152cdcbaf13d308ef4c[m
Author: Martin Bobek <martinbobek77@hotmail.com>
Date:   Mon May 8 11:30:51 2017 -0600

    Added a third test case.

[33mcommit eb1c567633d74e9c1a0ef03c59bd9336cbb53569[m
Author: Martin Bobek <martinbobek77@hotmail.com>
Date:   Mon May 8 11:21:25 2017 -0600

    Added testing interface. Fixed major bug.

[33mcommit 622925e56f28dee816ebc9d6b48ced4a56d47978[m
Author: Martin Bobek <martinbobek77@hotmail.com>
Date:   Sun May 7 23:24:40 2017 -0600

    Initial commit. Untested Follow computation.
