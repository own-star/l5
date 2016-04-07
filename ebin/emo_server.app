%% Feel free to use, reuse and abuse the code in this file.

{application, emo_server, [
	{description, "Cowboy Emo Server example."},
	{vsn, "1"},
	{modules, ['emo_server_app','emo_server_sup','l5_emo_server','toppage_handler']},
	{registered, [emo_server_sup]},
	{applications, [
		kernel,
		stdlib,
		cowboy,
		jsx
	]},
	{mod, {emo_server_app, []}},
	{env, []}
]}.
