# cse230-project
A terminal-based 4k VSRG, written in Haskell and brick. 

## Members
- David Li (A17966279, huantianad)
- Sean Yen (A16948219, SheepTester)
- put your name here
- put your name here

## Milestone 1: Proposal
Our intent is to create a rhythm game in the terminal using Haskell and brick. Specifically, we aim to create a generic 4-key vertial scrolling rhythm game, similar to existing games like osu!mania, Quaver, and DDR.

Archtectually, we are currently exploring the usage of the `proteaaudio` library to playback audio. The current roadblock is the inability to read the current audio position, to allow the game to be synced to the audio properly.

For storing chart data, we intend to use the `.osu` file format, used by the rhythm game osu. We will implement a cusom parser that reads the relevant data from the file. By not using a custom file format, we're able to reuse existing charts for other games.

The main difficulty will be the game itself. First, create a minimally working game, that properly reads user input, and can smoothly display the notes moving vertically down the screen. More description of how the game might work or looks in general? Maybe a better description of 4k in the first paragraph. Scoring system?
