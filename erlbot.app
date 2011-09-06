{ application, erlbot, [
	{ description, "Erlbot - Erlang IRC Bot" },
	{ vsn, "1.0" },
	{ modules, [ erlbot, bot, server, channel, foobar, irc ] },
	{ applications, [ kernel, stdlib ] },
	{ mod, { erlbot, [] } },
	{ start_phases, [] }
] }.