sig root.

accum_sig a { kind a_type => c_type }.
accum_sig b.

type      p     c_type.     % This does not work, but it should

type      q     a_type.     % This works, but it shouldn't

type      q     b_type.     % This does not work, but it should
