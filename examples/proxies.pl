
% this Prolog file contains some example of Logtalk parametric objects proxies
%
% Logtalk parametric object proxies are Prolog facts that can be interpreted
% as "instantiations" of the parametric object identifiers; in the case of 
% L-FLAT, proxies provide an alternative representation for simple languages
% and mechanisms
%
% if your back-end Prolog compiler doesn't support term_expansion/2, you
% will need to ensure that any sets (represented as lists) in your proxies
% are ordered (when defining languages and mechanisms in Logtalk source
% files, compilation with the option hook(hook) automatically ensures that
% the sets are ordered)

fa(s1, [s1/a/s1,s1/b/s2,s2/a/s2,s2/b/s1], [s1]).
