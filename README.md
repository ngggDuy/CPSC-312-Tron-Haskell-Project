[![Typing SVG](https://readme-typing-svg.demolab.com?font=Fira+Code&weight=500&size=27&pause=1000&width=435&lines=Welcome+to%3A+Tron)](https://git.io/typing-svg)

Our project, Tron, will be a recreation of the [Tron Light Cycle](https://en.wikipedia.org/wiki/Tron_(video_game)) game. The objective is to force enemy light cycles into walls and jet trails, while also avoiding them. The winner of the game is the last person standing.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team hotCakes

+ Eric Kuo - 58163288
+ Duy Nguyen - 95844189

## Link to Video
https://1drv.ms/v/s!Ate0x2ZGgORSjW4ssxbx8ppvU61_?e=yaRhvI

> Note: it may take a while for the video to load. If it does not load, please download and play the video.

## Full Proposal
Our full proposal can be found at [Proposal.md](/Proposal.md)

## MVP Documentation
In our proposal, we worked on representing the state of the game and building the core logic of handling collisions and updating states. For our final project, we wanted to challenge ourselves to build several complex features that would give us lots of exposure to developing games in Haskell, apply our learnings from lectures, and present us with an opportunity to learn new things! The following section describes the features we promised in our proposal.

### 1. A Graphical User Interface (GUI)
We used [Gloss](https://hackage.haskell.org/package/gloss) to create a basic GUI for our game. Gloss's [`playIO`](https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html#v:playIO) function was relatively easy to use; by simply defining functions to handle events, render images, and advance each step of our game, we were able to get a prototype up and running relatively quick.
- [`start`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L53-L64) - the main function for our `UI` module.

Furthermore, in our proposal, we created a data type, [`TronState`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L46-L54), to represent the state of our game. For our GUI, we created a wrapper around it called [`GameState`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L34-L38), analogous to the concept of a [“world”](https://hackage.haskell.org/package/gloss-game-0.3.3.0/docs/src/Graphics-Gloss-Game.html#:~:text=world%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20--%20%5EThe%20initial%20world%20state) in `Gloss`. Our game state contained additional metadata we felt was not **core** to the logic of our game, but information that was needed for our GUI. This included keeping track of the current mode, the score, and what move the player chose.
- [`Mode`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L25-L32) - algebraic data type for whether we’re in the menu, game-over page, score page, or the game is over (win or loss)

Here are some of the main components of our GUI:
1. Key event listeners
   - Our code not only listens to the arrow keys when the game is in progress, but it also responds to other keys to control different pages of our game such as the menu, score, and game-over page. Depending on the current page the user is in, different keys result in different actions. For example, when the game has finished, the user has the option to hit `r` to restart the game, `m` to navigate to the menu, or `ESC` to exit the game.
      - [`handleKey` - game is in progress](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L159-L167)
      - [`handleKey` - menu page](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L168-L177)
      - [`handleKey` - score page](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L178-L184)
      - [`handleKey` - game over page](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L185-L196)
2. Advancing each iteration of our game
    - The function [`handleStep`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L225-L239) advances each iteration of our game by one step. This was quite quick to implement because our proposal simply uses the functions we worked on in our proposal (listed below):
      - [`nextGameState`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L146-L156)
      - [`advanceCPUStateIO`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L167-L176)
3. Rendering each frame
    - `Gloss` provides lots of [helper functions](https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Data-Picture.html) to draw different shapes, text, and colours. Since our game has multiple pages to navigate to, we designed multiple functions to draw the scene depending on the current mode of the game. For example, if the user is in the menu, we call [`drawMenu`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L90-L96), if the game is in progress, we call [`drawGrid`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L129-L139). 
      - [`drawGameState`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/master/haskell/src/UI.hs#L83-L88) - calls the respective function depending on the mode of the game
    - One obstacle we encountered when drawing the grid is that the package [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html) uses 1-based indexing and the coordinate `(1,1)` as the **top left** of the grid. We used `Data.Matrix` to hold the state of our game. However, after lots of research, we found out `Gloss` uses `(0,0)` to represent the **center** of the window. Hence, this required several calculations to account for these discrepancies.

One big challenge we faced with `Gloss` were keys not being registered.
- When drawing frames, `Gloss` does not allow users to reuse the previous frame. Instead, the whole game state has to be drawn from scratch on every step of our game. Consequently, the arrow keys are sometimes unresponsive. Our theory is that our function [`drawGrid`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L129-L139) is expensive since our matrix is large; this causes some timing issues when `Gloss` tries to render each frame and listen to keyboard inputs.
- However, we prototyped several iterations of drawing our grid, each time minimizing how much was drawn.
    - Our first version of `drawGrid` drew empty cells as black rectangles. Unfortunately, this caused a lot of lag since our matrix of size `n x n` needed to draw `n x n` pictures.
    - Our [second attempt](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blame/24cca26ce79963f45b0f9355d5e91649cf60536b/haskell/src/UI.hs#L65-L72) only drew cells where the CPU and player had their jet trails occupied. This resulted in a much smoother game compared to our first version since we ignored empty cells, but we felt there was more that could be improved.
    - Our [third version](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L129-L139) reduced the size of the list we operated on and the number of times we accessed our matrix.
- > Note: the matrix represents the state of the game where 0 is an empty cell, 1 represents the players jet trail, and -1 represents the CPUs jet trail

### 2. CPUs Varying in Difficulty
Initially, we had one (dummy) CPU that only moved in a single direction. However, since we designed the necessary infrastructure in our proof-of-concept to support CPUs, all we had to do was add new algorithms with increasing difficulties. 

In our final project, we incorporated 2 different difficulty levels:
- Easy  
    - Our first CPU plays to survive. It looks exactly one step ahead in the future (either moving forward, left, right) and picks the move that doesn’t result in a loss. If multiple moves are valid, we take priority moving forward, then left, and lastly right. 
    - [`beginnerCPUAlgorithm`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L178-L189)
- Medium
    - Our second CPU is similar to our beginner CPU, however, it uses the [random](https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html) package to add some unpredictability when choosing a move.
    - [`mediumCPUAlgorithm`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L191-L208)
- Hard
    - Originally we were implementing a minimax algorithm for our Hard CPU, however, as our search tree grew too large, we discovered that a pruning algorithm was required to reduce the search runtime. This required us to refactor a significant portion of our code and so we decided to leave this feature out.

### 3. Abstracting and Refining our Code
We wanted to ensure that the core logic of our game had no side effects and that all our functions were total. We accomplished this by writing extensive tests simulating the possible states of our game and by leveraging the total functions defined in our package dependencies. For example, we made sure all our File IO operations covered as many edge cases as possible and used the total functions [`safeGet`](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html#v:safeGet) and [`safeSet`](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html#v:safeSet) from the [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html) package to safely lookup elements from our matrix. This helped us create more robust error handling whenever a player collides or goes out of bound.
- Code examples of using [`safeSet`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L131-L144) and [`safeGet`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L98-L101)
- [`getScores`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L66-L76) - ensuring our File IO operations do not result in errors by creating the file if it does not exist, using [`readMaybe`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-Read.html#v:readMaybe), etc.
- [Spec.hs](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/master/haskell/test/Spec.hs) - test cases focusing on lots of different scenarios and edge cases

### 4. Haskell's Module System
Like other programming languages, Haskell has support for a module system that enabled us to break our one long file into 2 separate modules: [Lib](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/master/haskell/src/Lib.hs) and [UI](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/master/haskell/src/UI.hs). This greatly improved the workflow and management of our code as it separated the logic of our game from the UI. As our code was less coupled, we could make patches that can be tested independently. We also learned how to use numerous modules from external packages and had fun reading about concepts such as qualified imports, import scopes, and hiding identifiers.

### Importance of the Problem the Project Addresses and Teams Language Learning
One of our main goals with Tron was to discover how feasible it is to create games in Haskell. More specifically, we aimed to answer questions surrounding how to:
- Enforce the rules of a game
- Efficiently represent the state of a program for CPUs to make decisions on
- Handle user-controlled events and progression 
- Navigate and update different states of a game

Our main takeaway is that the functional nature of Haskell makes it simple to concisely represent rule-based games. Although we faced some trouble at the start designing the game without OOP, enforcing the rules of the game was simple to code once we figured out how we wanted to represent the state of the game. We learnt that functional programming has many strengths when it comes to games. For example, passing around the state of our game without side effects helped us become more confident that our tests were working correctly and that the state of our game was being updated correctly. Using `Gloss` was simple as well, it didn’t require as much code or head-banging as we expected, instead, it was intuitive to use and only required slight modifications in our proposal to connect with `Gloss`. Lastly, Haskell’s pattern matching, immutability, and type safety made it quite easy to write code and debug errors.

### Significant Step Beyond Core Proposed MVP
- Multi-page GUI
    - In our proposal, we originally planned our MVP to have a simple GUI that when executed, would immediately start the game. However, we wanted to improve the user experience and were curious how easy it would be to define different pages in our game such as a menu, game-over page, etc.
    - We accomplished this by adding a [`Mode`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L25-L32) data type that we used to decide what to render to the screen and what keys were allowed to be pressed in that mode. For example, if the mode of the game is in progress, then we would draw the grid and only respond to arrow key events. On the other hand, if the mode of the game is the menu, then we would draw the menu and listen to other keys.
        - `handleKey` for the [menu](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L168-L177) and the [game in progress](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L158-L167)
        - drawing the [menu](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L90-L96) and the [grid](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L129-L139)
- Scoring system
    - To challenge ourselves, we wanted to come up with a scoring system for our game. In addition, since we’re learning about file systems in CPSC 313, we decided to save our scores to a file permanently so that when the game is launched, scores from previous games are loaded back in.
    - For the actual scoring mechanism, the score is determined by how long the user survives. More specifically, on each frame generated, the score is increased by a certain amount.
        - [updating the score](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L238)
    - Anytime the game finishes, we save the score to the file
        - [handling game over](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L185-L196) and eventually [adding the scores](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L78-L81)
    - When the user is in the menu and selects to view the scores, we read from the score file and choose the top 10 highest scores. We made sure our function had no side effects so that if the score file did not exist or was malformed (contains data that aren’t scores like random text or blank lines), the function would not crash. This was accomplished using [`readMaybe`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-Read.html#v:readMaybe) and helped our code be more robust.
        - [`getScores`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L66-L76)

### Progress Towards a Fully Polished Product
With a lot of the core functionality and UI of our project completed, we’ve made substantial progress towards a fully polished product. There are numerous tasks we would love to learn more about and believe would not require much work since our MVP and proposal have laid out most of the groundwork.

- Learning more ways to efficiently represent the state of a game - we would like to refactor the state of our game so that our code is more maintainable and extendable in the future. Throughout the project, when we tried adding new arguments to our data type, [`TronState`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L46-L54), we were forced to refactor a lot of areas in our code to account for these new fields. We had a lot of places that pattern matched like `TronState(_,_,_,_,_)` or had helper functions to get a single argument out of the state. One of our main ideas is to use [record syntax](https://devtut.github.io/haskell/record-syntax.html) to make our code more readable. 
  - Another idea we had is to create a new data type, `Metadata`, that could be used in our `TronState` to hold information like lives, scores, positions, etc. It would make it easy to add new arguments without requiring drastic changes for every function that depends on our data type.
- Furthermore, we ran into issues implementing a complex AI algorithm as `TronState` stored both the game's metadata as well as the game board. As such, it became expensive and cumbersome when generating search trees. In the future, we could potentially split `TronState` to accommodate for these resource-intensive algorithms. 
- Better UI framework - for a fully polished product, we would like to replace `Gloss` with a library that has more styling options (`Gloss` only allows a single font and a limited number of shapes) and to also address the problem we faced with keys sometimes not being registered. This would not require much work since we could reuse existing work from `Gloss` and adapt it to the new package we choose.
- Minor modifications - since we have the basic architecture and functionality of our code done, we can enhance our game by adding smarter and **multiple** CPUs, lives, and even introducing PvP gameplay. These would not only make our game more enjoyable but would also serve as excellent learning opportunities.

### How and Why Our Final Project Varies In Limited and Reasonable Ways from the Proposal
In our proposal, we worked on modelling the state and interactions of the game. This involved defining how the state updates when we move forward/left/right and defining the logic for handling jet trail collisions and out-of-bounds. In our final project, we built on top of the work from our proposal to accomplish several challenging features. Learning how to make a GUI gave us exposure to a whole new area in Haskell dealing with IO, graphics, event handling and world transitions. Moreover, working on CPU algorithms allowed us to **use** the state we came up within our proposal to help our CPUs make more informed decisions. Lastly, our bonus features, the multi-page GUI and scoring system, helped us learn a lot of new concepts such as working with file systems in Haskell.

## Application of New Learning
- Matrices
    - We used matrices extensively in our project to model the current state of the game. We used the `Data.Matrix` package which helped us with the following features:
        - Accessing/modifying elements of a matrix - [`safeSet`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L131-L144) and [`safeGet`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L98-L101)
        - Initializing zero matrices - [`zero`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L75)
        - Printing matrices for our terminal-based game in our proposal - [`prettyMatrix`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L60)
    - Using this package gave us lots of exposure to [hackage](https://hackage.haskell.org/) and how to navigate the documentation for packages. Our project also benefited from this package because it simplified how we represented our game compared to using 2D lists or vectors and having to define our own total functions for the use cases mentioned above. It was also very easy for us to create thorough and easy-to-read test cases.
      - [`fromLists`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/test/Spec.hs#L198-L207) creates a matrix from a list
- Learning `Tasty` and writing comprehensive tests
    - We learned that writing tests are just as important as writing the business logic of our game. Without tests, we wouldn’t have caught a lot of bugs in our code and it would have slowed down our development. Our project benefited from tests because it made us more confident that our code and state behaved as expected, and we could spend more time focusing on delivering the features we promised in our MVP, rather than debugging.
    - Examples of our test cases:
      - [jet trail collision tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L97)
      - [moving forward tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L260)
- Working with state
    - As mentioned earlier, we learned a lot about making careful design decisions when it comes to the state of our game. By capturing all the information we needed for our game or UI in a data type, we could leverage pattern matching and type safety to help write code more efficiently. Furthermore, passing around our state as arguments to our functions made it easier to debug and test our code. 
        - [`TronState`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/Lib.hs#L46-L54)
        - [`GameState`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L34-L38)
- IO
    - We gained a lot more exposure to working with IO in our MVP. From lectures, we learned about IO Monads and this allowed us to improve our understanding of why they’re descriptions or recipes of effectual computations. Our project benefited from this because of how we no longer have side effects and could create more complex features.
    - We learned about the [`do`](https://en.wikibooks.org/wiki/Haskell/do_notation) notation, working with File IO, and employed it in multiple areas of our code such as
      - [`getScores`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L66-L76)
      - [`start`](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/438b5c3207d9da02bfdea1dffef0f5ffa2ef7747/haskell/src/UI.hs#L53-L64)
- We could list a whole lot more of our new learning, but the points mentioned above summarize the major learning opportunities we had as well as other sections in the README which talk about other learnings and how our project benefited from them! 

## Running our MVP

To run the code:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run the following commands
    - `stack build`
    - `stack ghci`
    - `:l src/UI.hs`
    - `start`
- Remarks and rules:
    - Running `start` will cause a new window to pop up and will bring you to the main menu.
    - Select the difficulty of the CPU you want to play against.
    - After the game is over, you will be prompted to try again, return to the menu, or exit the game!
- Controls
    - Once the game starts, you (the blue player) will use the arrow keys to control the direction of your lightbike.

    > :warning: As mentioned earlier in the section *"One big challenge we faced with `Gloss` were keys not being registered"*, the arrow keys may not always respond. This is a limitation with
    > the `Gloss` package and how frames are rendered. To avoid this issue, you may need to press a key multiple
    > times to turn in the desired direction.

    - As the game progresses, your jet trail will grow and the objective is to avoid colliding into the walls or the jet trails (your own and the CPUs)!
    - Some interesting cases to try
        - What happens if a player collides into their jet trail?
        - What happens if a player travels out of bounds?
        - Can you predict where the CPU will go next? Try to outsmart them and move on to the next level of difficulty!
        - Is it better to play aggressive or passive against the CPU?

To run the tests:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run `stack test`
- Some of our main test groups testing core logic of our proof of concept
    - [jet trail collision tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L97)
    - [out of bounds tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L225)
    - [moving forward tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L260)
    - [moving right tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L351)
