<p>
    <a href="https://github.com/DenverCoder1/readme-typing-svg">
      <img src="https://readme-typing-svg.herokuapp.com?font=consolas&color=%2311A1F7&size=30&height=45&lines=Welcome to: Tron" />
    </a>
</p>

Our project, Tron, will be a recreation of the [Tron Light Cycle](https://en.wikipedia.org/wiki/Tron_(video_game)) game. The objective is to force enemy light cycles into walls and jet trails, while also avoiding them. The winner of the game is the last person standing.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team:

+ Eric Kuo
+ Duy Nguyen

## Product Pitch

<img src="https://i.pinimg.com/originals/56/e4/1b/56e41b45b51feda58b668cdd4c9a0ff2.gif" width="400"/> <img src="https://i.pinimg.com/originals/a6/78/cf/a678cfba3e2f13ff3afc4bde10bb29dd.gif" width="400">

Cool right?!? You’ve got to admit-we’ve all wanted to ride a [light cycle](https://tron.fandom.com/wiki/Light_Cycle_(1st_generation)) before, even if this is your first time hearing about it. Tron was one of our favourite childhood movies growing up and as kids, we’ve always thought this was what actually happened inside our machines; a world existing within chips and circuits. 

### Objective
The objective of the game is simple: a player and CPU will keep moving in a grid as time advances, leaving a trail (“jet trail”) behind them wherever they go. A player wins when their opponent crashes into either the boundaries or any jet trail (their own or the CPUs). To win, players must avoid crashing and maneuver instead to force the opposing player to crash.

### Our Vision
Our vision for the game is a series of challenging levels growing in the number and difficulty of CPU players. We would also model the UI and graphics of our game similar to that of the movie. In the far-out future, we hope our game can support multiplayer scenarios over the network or on the same machine. Players can compete against each other to climb in rankings based on the number of games won!


### Motivation and why this matters to us
After our lecture about the Magic Sum game, Duy and I were curious not only about how feasible Haskell is for creating complex games but were inspired to look deeper into the following concepts:
- How can we enforce the rules of a game (e.g. where a player can move)?
- How can we efficiently represent the state of a program for CPUs to make decisions on?
- How can we handle user-controlled events and progression?
- How easy is it to navigate/update between different states of a game in Haskell?

We realized these are common design questions people would have when developing a game with the above requirements. As a result, we wanted to combine our love for Tron and Haskell to explore these questions. Living in the world of Tron and riding a light cycle has always been a dream, but in the meantime while you dream it, we'll build it! Also, this is perfect if you have 10 minutes to kill or you're bored in lecture. (Make sure your prof doesn't catch you!)

## Minimal Viable Project
With most of the groundwork for our game complete, we can focus more time on our MVP to really leverage the features of Haskell and to take our project to the next level.
We plan on refining the following concepts and incorporating the specified features:

### CPUs varying in difficulty
As of right now, we only have a Beginner (Dummy) CPU which moves in the same direction the whole time. We plan on adding more complex algorithms that will allow our CPUs to become more challenging. Since we already added functionality in our proof-of-concept to support CPUs of varying difficulties, this is only a matter of adding new algorithms.

This could involve concepts we learned in class about game trees, next moves, min-maxing, or new algorithms that we discover/create!

### Abstracting and refining our code
We plan on leveraging concepts we learned in class about Haskell to make our code more readable, performant, and to also build upon the concepts from the Magic Sum lecture such as using type classes to represent our game state. 

Furthermore, since there are no side effects in Haskell, things like passing around state and handling collisions with jet trails/walls will be much easier to test and help us be more confident that things are working correctly. We also plan on taking advantage of how easy it is to define total functions in Haskell. For example, the [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html) package has implementations of [safeGet](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html#v:safeGet) and [safeSet](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html#v:safeSet). This will allow us to define total functions involving matrices, which will also help us create more robust error handling whenever a player collides or goes out of bound.

Finally, we also want to be able to create objects that have a functional nature and with the strongly typed features of Haskell, we can reduce common type mistakes when dealing with large data types by leveraging compile-time errors and function signatures.

### Haskell’s module system
We can also take advantage of Haskell’s module system by separating our code into different modules like a player module, CPU module, game state module, and more. This will lead to a more manageable and less coupled code that can be tested independently and easily modified in the future.

### Packages! - Matrices, listening to user inputs, UI
Lastly, like every game, we want to add visuals! This will naturally lead us to learn and apply some new package/concepts of the language we have not touched before. In addition to the `Data.Matrix` package mentioned in our proof of concept, we are curious to learn how keyboard events are handled in Haskell and how graphics are rendered. Some packages our group has been looking into are `Gloss` and `System.Console.ANSI`. We can't wait to play around to make our game more exciting!

## Proof of Concept
Our proof-of-concept focuses on the logic and functionality of the game Tron. In more detail, it consists of the following:

### 1. A way to represent the current state of the game
At a high level, our game state includes a matrix that stores where the jet trails currently are, the direction and position of players, the difficulty of the CPU, and whose turn it is. This involved creating [several data types and type synonyms](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L16-L44). We also decided to use the library [Data.Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html) to represent the state of the game. Using this package gives us an opportunity to work with something new in Haskell and to leverage the utility functions that operate on a matrix out of the box.

The main idea is that the initial game state starts with a zero matrix, where zero indicates a cell is unoccupied, free for any CPU/player to travel. As time progresses and the players move, a 1 signifies the jet trail of the current player, and a -1 signifies the jet trail of the CPU. In our MVP, the idea is to use the numbers in this matrix as a way to color our cells graphically.

- Data type for our game state: [TronState](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L46-L54)
- [initTronState](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L65-L73) initializes our `TronState`

### 2. A way to advance the current state of the game
We implemented logic to allow a player and CPU to specify where they want to move next (either left, forward, or right). By knowing the current state of the game, and where a player wants to move, we can appropriately update the matrix and each player’s position and direction.
- [nextGameState](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L148-L160) advances the state based on the specified `Move`
- [moveLeft](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L124-L126), [moveRight](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L128-L131), [moveForward](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L133-L146) allow us to update the current state

### 3. Collision and bound logic
We prototyped functionality that will allow the state of the game to detect when a player crashes into a wall, and when a player will collide into any jet trail (their own or the CPU’s). This will allow us to detect when the game is over, and who has won.
- [Jet Trail Collision logic](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L101-L108)
- [Out of Bounds logic](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L96-L99)

### 4. IO

We wanted some way for users to interact with our proof of concept. This was accomplished by allowing users to specify in the terminal which direction they wanted to go and to visually see the state of the game.
We prototyped this behaviour in the function [printNextGameState](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/src/Lib.hs#L179-L216) using recursive IO to pass the state around and to print out the player and CPU moves.

> Sneak Preview! Refer to instructions below to see how to run our code.
<img src="https://media.github.students.cs.ubc.ca/user/1272/files/e54d2280-329b-11ec-9215-78f3e097ac1b" width="300">

### How this illustrates a key element of our project
By laying out the groundwork for how we want to represent the state of our game and the various actions players/CPUs can execute in our game, we can be more confident in ensuring that the core logic of our game works and is feasible to implement. For prototype purposes, we prioritized our time in ensuring the logic and interaction of the game was correct, and experimented with various libraries/packages, leading to our choice of using Data.Matrix.

We were also curious about how to model direction, players, and moves, which are crucial aspects of any game that needs to be aware of their sense of direction. Hence, this led to several algebraic data types and some type-synonyms.

### How this gives us the confidence to complete our MVP
By having our core logic implemented and thoroughly tested code, we can focus more of our time and resources on exploring different libraries for UI like Gloss, how to read keyboard inputs, and refining our code to take on more Haskell features. 

Furthermore, after spending time prototyping with several external packages to represent the state of the game, we are more confident now with managing and using packages in our code.

### How to test and run the code: Haskell

To run the code:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run the following commands
    - `stack build`
    - `stack ghci`
    - `:l app/Main.hs`
    - `main`
    - Remarks:
        - Running `main` will execute our proof of concept for the game Tron
        - You can choose from the specified commands what to do next, each instruction will advance the game state into its next state based on the command provided
        - Each entry prints two things: your move and where the CPU decided to move.
        - Some interesting cases to try
            - What happens if a player collides into their own jet trail?
            - What happens if a player travels out of bounds?
- Other interesting cases to try
    - Make sure to execute the following instructions first
    - `stack ghci`
    - `:l src/Lib.hs`
    - Try
        - `initTronState` - see how the state is represented in our game!
        - `printNextGameState initTronState` - this is what `main` is calling! We used recursive IO to implement this

To run the tests:
- `cd` into the `haskell` directory
- open VS Code on that directory (`code .`)
- run `stack test`
- Some of our main test groups testing core logic of our proof of concept
    - [jet trail collision tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L97)
    - [out of bounds tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L225)
    - [moving forward tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L260)
    - [moving right tests](https://github.students.cs.ubc.ca/er11k26/cpsc-312-project/blob/c4e8985cf9737eb3734ab4ec770a3a93b2e3f4de/haskell/test/Spec.hs#L351)

## A road down memory lane
As always, we can't forget our basics! We had a great time following the HtDW recipe from CPSC 110
<img src="https://media.github.students.cs.ubc.ca/user/1272/files/e46cbe80-32a4-11ec-9f52-22da8ddb2d10" width="500">

## Legacy notes from original markdown
As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!

