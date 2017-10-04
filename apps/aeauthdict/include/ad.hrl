-define(BALANCE_0, $0).
-define(BALANCE_L, $\x7f).
-define(BALANCE_R, $1).
-define(HASH_BITS, 256).
-define(VALUE_BITS, none).

-define(NEG_INFINITY_KEY, binary:copy(<<0:8>>, 32)).
-define(POS_INFINITY_KEY, binary:copy(<<16#ff:8>>, 32)).

%% Proof packaging
-define(LEAF_IN_PACKAGED_PROOF, 2:8).
-define(LABEL_IN_PACKAGED_PROOF, 3:8).
-define(END_OF_TREE_IN_PACKAGED_PROOF, 4:8).

%% For type testing
-define(LEAF_NODE, #{value := _}).
-define(INTERNAL_NODE, #{balance := _}).
-define(PROVER_LEAF, #{value := _, is_new := _}).
-define(PROVER_INTERNAL_NODE, #{balance := _, is_new := _}).
-define(VERIFIER_LEAF, #{value := _}).
-define(VERIFIER_INTERNAL_NODE, #{balance := _}).
-define(LABEL_ONLY_NODE, #{label := _}).

%% TODO: use blake2b
-define(KEY_BITS, ?HASH_BITS).
-define(DIGEST_BITS, ?HASH_BITS + 8).
-define(HASH(X), aec_sha256:hash(X)).
