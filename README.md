# Haskell Errors

Contribution from a newbie, copied from the discussion list. If the README is not an appropriate place for this note, well, feel free to put it somewhere else.

Discourse contributor: ““Variable not in scope: x” are pretty helpful and understandable for beginners”

Yakushima: The first time I got that message I was in ghci. I was deeply puzzled. It took a couple minutes for it come to me: Oh, ghci’s REPL is saying that the variable is not in ITS scope.

So I’d rewrite it something like this:

“Variable not in scope on the ghci command line: x. If it exists in some module, declare it in the appropriate module declaration.”

Better yet, though of course it means more diagnostic code before emitting the error message:

Offer a list of modules in the modules being developed (not the code being loaded from other sources) where that variable may exist. If it doesn’t exist under that variable name in those modules that would be a good thing to say.

You may respond, “Well, it was just a few minutes out of your life. So what?” But when people are learning a language that already challenges their existing programmer intuitions on a number of levels, challenging it on the level of mere error messages is making it worse than if they weren’t learning such an unusual language. In some sense, the “user experience” of Haskell for the newbie is inherently hard, it’s in the language itself, and there’s not much to be done about the language aspect. But that means error messages are an important focus, because that where the UX leverage is to be had. Even if it would be overkill of obviousness for, say, Typescript, it wouldn’t be for Haskell.

As for “avoid success at all costs” being unrelated to error messages, I think it is related. If you drift off into your own nomenclature, and everyone involved in the discussion is in a community where everyone else knows what terms mean, you’ll write error messages using that nomenclature and it will stump the newbies. It may not be an intended consequence of “avoid success at all costs”. But the world is full of unintended negative consequences caused by working with good intentions.
