-module(gitto_vsn).

decode_revision({tag, Tag}) ->
    ok;
decode_revision({branch, Branch}) ->
    ok;
decode_revision(Revision) ->
    ok.
