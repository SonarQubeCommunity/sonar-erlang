-module(noemacsstyle).
-export(hello/0).

hello()->
	Code,
	SomeList = [
            Element1
            , Element2 %first issue
            , Element3 %second issue
        ],
 	%comment
	DifferentBlock,
	%comment
	Whatever1 = [
            Element1,
            Element2
            ,Element3 %third issue
        ],
	%comment
	Whatever2= [
            Element1,
            Element2,
            Element3
        ].
