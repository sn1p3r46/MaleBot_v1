# MaleBot
MaleBot(@malebot) is a telegram bot, which scolds you if you use bad-words. It supports both private conversation and groups, but for now can identify only italian bad-words.

### Instruction
To work you need to create a file `src/token.hrl` where define telegram token:
```
	-define(TOKEN, "<token>").
```
To get the token follow the instructions here [how-do-i-create-a-bot] (https://core.telegram.org/bots/#3-how-do-i-create-a-bot)

### Run:
	./start_bot.sh

### Technology:
- Erlang

## License
MaleBot is released under the [MIT License](http://www.opensource.org/licenses/MIT).

This software is based on [raboter](https://github.com/radist101/raboter)

This software use [Parole Italiane]( https://github.com/napolux/paroleitaliane/blob/master/paroleitaliane/lista_badwords.txt)
