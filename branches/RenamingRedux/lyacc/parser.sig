sig parser.
accum_sig lyaccshares, blists, maps,control,string.

% Globally-accessible functions for use by the LambdaYacc module
type stream_name out_stream -> o.
type gen_first out_stream -> int -> (list o) -> o.
type rrdeterministic (list ch) -> (list o) -> o.
type srdeterministic (list ch) -> (list ch) -> (list o) -> out_stream -> o.
type fsbrc (list ch) -> o.
type gen_reduce out_stream -> ch -> o.