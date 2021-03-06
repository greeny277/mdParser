# mdParser

Author
---

Name: Christian Bay

Mail: christian.bay [at] posteo.de

Tools
---

- Building tool [stack](https://docs.haskellstack.org/en/stable/README/) (Version 1.7.1)
- Documentation tool [haddock](https://www.haskell.org/haddock/) (Version 2.18.1)

Usage
---

To build the tool use:

``` bash
stack build
```

To exec the tool use:

``` bash
cat SomeMarkdownfile.md | stack exec mdParser-exe
```

About
---

Another program I created because of an job interview. The task was to parse a Markdown file
to HTML in an hour. When I was in the interview I started using Haskell but rejected the idea due a
lack of time and switched back to Java.
A day after the interview I wanted to prove for myself that I'm able to solve the task
by using Haskell. So I did. I was able do solve the task in round about 3-4h. The parser is able
to parse the use-cases mentioned by the [Wikipedia](https://en.wikipedia.org/wiki/Markdown) page.
I am sure that there are a few corner cases I missed while testing the parser.

Obviously this has been implemented about a dozen times, for instance by
[pandoc](http://pandoc.org/) (a tool I can highly recommend). Nevertheless, it is a good opportunity
to come in touch with Haskell's [Parsec](http://hackage.haskell.org/package/parsec) library or to
reinforce your knowledge.
