# cse230-project
A terminal-based 4k VSRG, written in Haskell and brick. 

## Members
- David Li (A17966279, huantianad)
- Sean Yen (A16948219, SheepTester)
- Gary Lin (A16915179, Atlae)
- put your name here

## Milestone 1: Proposal

<img
  src="https://repository-images.githubusercontent.com/123398967/40a2f200-be6f-11eb-9255-25474eebac8a"
  alt="Various examples of a 4k VSRG"
  width="400"
  align="right"
/>

We intend to create a rhythm game in the terminal using Haskell and [`brick`][brick]. Specifically, the game will be a 4-key vertial scrolling rhythm game (4k VSRG) similar to existing games like [osu!mania][osu], [Quaver][quaver], and [DDR][ddr], where indicators glide down from the top, following the music's rhythm, and the goal is to press the corresponding key right as it lands on the bottom.

Architecturally, we are looking at using [`proteaaudio`][proteaaudio] to play audio. A major problem is the inability to read the current audio time to sync the game to the audio precisely.

For storing level data, we will implement a custom parser for the [`.osu`][osufile] file format, used by the rhythm game [osu][osu]. By using a standard file format, we're able to reuse existing levels made for other games.

There are many challenges to making the game itself. Our goal is to create a minimally working game that properly reads user input and can smoothly display the notes moving down the screen. We will then settle on more specific details, such as:

- The overall appearance of the game
- The scoring system
- Specific game mechanics
<!-- More description of how the game might work or looks in general? Maybe a better description of 4k in the first paragraph. Scoring system? -->

[brick]: https://hackage.haskell.org/package/brick
[proteaaudio]: https://hackage.haskell.org/package/proteaaudio
[osufile]: https://osu.ppy.sh/wiki/en/Client/File_formats/osu_%28file_format%29
[osu]: https://en.wikipedia.org/wiki/Osu!
[quaver]: https://quavergame.com/
[ddr]: https://en.wikipedia.org/wiki/Dance_Dance_Revolution
