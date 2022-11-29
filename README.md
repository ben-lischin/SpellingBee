# Spelling Bee

The game presents 7 letters, from which you can create words\
The central letter is special, so we have colored it purple

To interact, you may type through your keyboard. Only the available 7 letters will register. You can backspace if necessary, and hit return to enter a word (will only work if the word is valid).

A valid word must...\
• use only the available letters (can repeat)\
• contain 4+ letters\
• contain the special (purple) letter\
• be a word in the dictionary (for our purposes, in words.txt)\
• not be a duplicate of an already-entered word

Scoring works as follows:\
• 1 point awarded for a 4-letter word\
• +1 point for every additional letter beyond the first four\
• +7 bonus points for entering a panagram (uses all 7 letters)



https://user-images.githubusercontent.com/94412143/204410176-15ffe6d9-b32b-4cda-9d8a-6ded9cb6b09a.mp4



Note: the current game is set up to use the letters a,r,s,e,t,i,h\
You can change this by altering the list of letters, LETTERS-1, on line 112 of SpellingBee.rkt to be whichever 7 letters you would like (the 4th letter will be interpreted as the special letter)
