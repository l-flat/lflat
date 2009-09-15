/* 
	Parsing a jflap file to another (lflat) data format
	
	Author Paul Crocker
	
	September 2009
	
	Version 1 - Finite Automatas
	
	An example making use of the xml contribuition in Logtalk
	
	user_input, user_output and user_error are the standard i/o streams in prolog

	To Use compile the necessary files with logtalk_load then use one of the four queries as shown below.
	The first threee simple show the use of the xml parser and create a new xml file with an intermediate format
	The last query converts a jflap file to a lflat format ready for compiling - note that 	when converting 
	it will print out to stdout the initial,final states and transitions in the lflat Format
	
	logtalk_load(xml_parser(loader)).
	logtalk_load(xmljflap).
	extractjflap( istates, 'nfa.jff').
    extractjflap( fstates, 'nfa.jff').
    extractjflap( states, 'nfa.jff').

    %load and compile l-flat
    %move to the directory with a jflap file to convert (version 1 - finite automata only)
    convertjflap(lflat, 'nfa.jff').
	then try 
	[nfa.lgt].
	nfa::show.
 */


% convert to intermediate xml ResultElement - create xml of this and then convert to native format
convertjflap( Query, FileName ) :-
	xml_query( Query, FileName, ResultElement , InitialS, FinalS, Trans),
	% Parse output XML into the Output chars
	xml::parse( Codes, xml([], [ResultElement]) ),
	atom_concat( FileName, '.xml', XmlOutputFile ),
	% Write OutputFile from the Output list of chars
	open( XmlOutputFile, write, XmlOutput ),
	put_codes( Codes, XmlOutput ),
	close( XmlOutput ),
	% Now Open File for Native Format Output
	file_name_extension(X,'jff',FileName),
	file_name_extension(X,'lgt',OutputFile),
	open( OutputFile, write, Output ),
	write(Output, ':- object('),
	write(Output, X),
	write(Output, ', instantiates(fa)).\n' ),
	
	pp_initial(InitialS, Output),
	pp_final(FinalS, Output),
	pp_delta(Trans, Output),
	
	write(Output, ':- end_object.\n'),
	close( Output ),!.

%with the data in intermediate format we can now output it how we like

xml_query( lflat, FileName, element( results, [], [InitialS,FinalS,Trans]), InitialS,FinalS,Trans ) :-
	writeln('Converting Jflap'),
	element_name( Structure, structure ),
	element_name( Automaton, automaton ),
	element_name( Initial, initial ),
	element_name( Final, final ),
	element_name( Transition, transition) ,
	input_document( FileName, Jflap ),
	findall(
		element('initialId', [id=Id], [] ),  									% creates an element called initialId
		(
			xml::subterm( Jflap, Structure ),
			xml::subterm( Structure, Automaton ),
			xml::subterm( Automaton, element(state,AttributeList,ElementList)),  % list of Attributes and list of elements
			xml::subterm( ElementList, Initial),
			list::member( id=Id, AttributeList )
		),
		InitialS
	),
	pp_initial(InitialS, user_output),
	findall(
		element('finalId', [id=Id], [] ),  
		(
			xml::subterm( Jflap, Structure ),
			xml::subterm( Structure, Automaton ),
			xml::subterm( Automaton, element(state,AttributeList,ElementList)),  %%list of Attributes and list of elements
			xml::subterm( ElementList, Final),
			list::member( id=Id, AttributeList )
		),
		FinalS
	),
	pp_final(FinalS, user_output),
	findall(
		Transition,  
		(
			xml::subterm( Jflap, Structure ),
			xml::subterm( Structure, Automaton ),
			xml::subterm( Automaton, Transition)
		),
		Trans
	),
	pp_delta(Trans, user_output),
	
	writeln('converting jflap Finished.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
% predicates to perform some simple textual extractions from a jff file.
%
extractjflap( Query, Filename ) :-
	xml_query( Query, Filename, ResultElement ),
	% Parse output XML into the Output chars
	xml::parse( Codes, xml([], [ResultElement]) ),
	atom_concat( Query, '.xml', OutputFile ),
	% Write OutputFile from the Output list of chars
	open( OutputFile, write, Output ),
	put_codes( Codes, Output ),
	close( Output ),
	% Pretty print OutputXML
	write( 'Output XML' ), nl,
	xml::pp( xml([], [ResultElement]) ).


% Extract all the states as a list of XML elements inside result
xml_query( states, Filename, element( results, [], States) ) :-
	writeln('All States'),
	element_name( Structure, structure ),
	element_name( Automaton, automaton ),
	element_name( State, state),
	input_document( Filename, Jflap ),
	findall(
		State,
		(
			xml::subterm( Jflap, Structure ),
			xml::subterm( Structure, Automaton ),
			xml::subterm( Automaton, State)
		),
		States
	).

% Extract all the initial states as a list of XML elements inside result		
xml_query( istates, FileName, element( results, [], InitialStates) ) :-
	writeln('InitialStates'),
	element_name( Structure, structure ),
	element_name( Automaton, automaton ),
	element_name( State, state),
	element_name( Initial, initial),
	input_document( FileName, Jflap ),
	
	findall( 
		State,
		(
			xml::subterm( Jflap, Structure ),
			xml::subterm( Structure, Automaton ),
			xml::subterm( Automaton, State),
			xml::subterm( State, Initial)
		),
		InitialStates
	).

% Extract all the finalstates as a list of XML elements inside result
xml_query( fstates, Filename, element( results, [], FinalStates) ) :-
	writeln('FinalStates'),
	element_name( Structure, structure ),
	element_name( Automaton, automaton ),
	element_name( State, state),
	element_name( Final, final),
	input_document( Filename, Jflap ),
	
	findall( 
		State,
		(
			xml::subterm( Jflap, Structure ),
			xml::subterm( Structure, Automaton ),
			xml::subterm( Automaton, State),
			xml::subterm( State, Final)
		),
		FinalStates
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	



% Native Format Element to text conversions


% AUTOTALK
%
%
pp_initial( [],_ ).		
	
pp_initial( [element('initialId', [id=Id], [] )|T] ,Stream) :-
	write(Stream,'initial(' ),
	put_codes( Id, Stream ),
	write(Stream,').\n'),
	pp_initial(T,Stream).

pp_final( List, Stream) :-
	write(Stream, 'finals(['),
	pp_final_lflat(List, Stream),
	write(Stream, ']).\n').
	
pp_final_lflat( [] , _).
pp_final_lflat( [element('finalId', [id=Id], [] )], Stream ) :-
	put_codes( Id, Stream).
	
pp_final_lflat( [element('finalId', [id=Id], [] )|T], Stream ) :-
	put_codes( Id, Stream),
	write(Stream,','),
	pp_final_lflat(T,Stream).

pp_delta( List, Stream ) :-
	write(Stream,'transitions(['),
	pp_delta_lflat(List,Stream),
	write(Stream,']).\n').

pp_delta_lflat( [] , _).

pp_delta_lflat( [H] , Stream) :-
	xml::subterm( H, element(to,_,     [pcdata(ToText)] )  ),
	xml::subterm( H, element(from,_, [pcdata(FromText)] )  ),
	xml::subterm( H, element(read,_, L )  ),
    text_value( L, ReadText ),
	(
		isEpsilon(ReadText) -> 
			write_epsilon(FromText,ToText,Stream)
			; 
			xml::subterm( H, element(read,_, [pcdata(RText)] )  ),
			write_delta(FromText,ToText,RText,Stream)
	).


pp_delta_lflat( [H|T] , Stream) :-
	xml::subterm( H, element(to,_,     [pcdata(ToText)] )  ),
	xml::subterm( H, element(from,_, [pcdata(FromText)] )  ),
	xml::subterm( H, element(read,_, L )  ),
    text_value( L, ReadText ),
	(
		isEpsilon(ReadText) -> 
			write_epsilon(FromText,ToText,Stream)
			; 
			xml::subterm( H, element(read,_, [pcdata(RText)] )  ),
			write_delta(FromText,ToText,RText,Stream)
	),
	write(Stream,',\n'),
	pp_delta_lflat(T,Stream).


isEpsilon([]). %just check its empty

write_delta(FromText, ToText, ReadText, Stream) :-
	put_codes( FromText, Stream),
	write(Stream,'/'),
	put_codes( ReadText, Stream),
	write(Stream,'/'),
	put_codes( ToText, Stream).

write_epsilon(FromText, ToText, Stream) :-
    put_codes( FromText, Stream),
	write(Stream,'/'),
	write(Stream,'[]/'),
	put_codes( ToText, Stream).


%%%lflat


%% Some Stuff Needed by the above

text_value( [(Text)], Text ).
text_value( [cdata(Text)], Text ).
%note that we need to deal with the empty list case...
text_value([],[]).
text_value( [pcdata(Text)], Text ).

element_name( element(Name, _Attributes, _Content), Name ).

/* input_document( +File, ?XML ) reads File and parses the input into the
 * "Document Value Model" XML.
 */
input_document( File, XML ) :-
	% Read InputFile as a list of chars
	open( File, read, Input ),
	get_codes( Input, Codes ),
	close( Input ),
	% Parse the Input chars into the term XML
	xml::parse( Codes, XML ).

put_codes( [], _ ).
put_codes( [Code|Codes], Output ) :-
	put_code( Output, Code ),
	put_codes( Codes, Output ).
	
get_codes( Input, [Code|Codes] ) :-
	\+ at_end_of_stream( Input ),
	get_code( Input, Code ),
	get_codes( Input, Codes ).
get_codes( _, [] ).
