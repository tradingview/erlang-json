

#Module json#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


This is a json encode/decode library for erlang.



Copyright (c) 2011 YAMASHINA Hio,
2011 Paul J. Davis

__Authors:__ YAMASHINA Hio ([`hio@hio.jp`](mailto:hio@hio.jp)), Paul J. Davis ([`paul.joseph.davis@gmail.com`](mailto:paul.joseph.davis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-binary_text">binary_text()</a>##



<pre>binary_text() = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a></pre>


  Json encoded text represented in erlang binary term.


###<a name="type-decode_error">decode_error()</a>##



<pre>decode_error() = term()</pre>


  reason for decode() error.


###<a name="type-decode_option">decode_option()</a>##



<pre>decode_option() = allow_comments | {allow_comments, boolean()} | {key_decode, existent_atom | binary | [existent_atom | binary | ignore]}</pre>


  option for decode().
see decode/2.


###<a name="type-decode_options">decode_options()</a>##



<pre>decode_options() = [<a href="#type-decode_option">decode_option()</a>]</pre>


  options for decode().
see decode/2.


###<a name="type-encode_error">encode_error()</a>##



<pre>encode_error() = term()</pre>


  reason for encode() error.


###<a name="type-json_array">json_array()</a>##



<pre>json_array() = <a href="#type-json_array">json_array</a>(<a href="#type-value">value()</a>)</pre>


  json array value is represented in erlang array term.


###<a name="type-json_array">json_array()</a>##



<pre>json_array(T) = [T]</pre>


  json array value is represented in erlang array term.


###<a name="type-json_number">json_number()</a>##



<pre>json_number() = integer() | float()</pre>


  json number value is represented erlang integer or float term.


###<a name="type-json_object">json_object()</a>##



<pre>json_object() = <a href="#type-json_object">json_object</a>([{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}])</pre>


  json object.


###<a name="type-json_object">json_object()</a>##



<pre>json_object(Pairs) = {Pairs}</pre>


  json object value is represented in erlang tuple which contains single proplist style value.


###<a name="type-json_primary">json_primary()</a>##



<pre>json_primary() = <a href="#type-json_string">json_string()</a> | <a href="#type-json_number">json_number()</a> | boolean() | null</pre>


  non-structured values.


###<a name="type-json_string">json_string()</a>##



<pre>json_string() = <a href="unicode.md#type-unicode_binary">unicode:unicode_binary()</a></pre>


  json string value is represented erlang binary term.


###<a name="type-key">key()</a>##



<pre>key() = <a href="#type-json_string">json_string()</a> | atom()</pre>


  key of object value. key() is mostly binary(), but it may be in atom() depended on key_decode option.


###<a name="type-text">text()</a>##



<pre>text() = <a href="unicode.md#type-chardata">unicode:chardata()</a></pre>


  Json encoded text represented in erlang chardata term.


###<a name="type-value">value()</a>##



<pre>value() = <a href="#type-json_primary">json_primary()</a> | <a href="#type-json_object">json_object</a>([{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]) | <a href="#type-json_array">json_array</a>(<a href="#type-value">value()</a>)</pre>


  any type of json value.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>decode json text into erlang term.</td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td>decode json text into erlang term.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>encode erlang term into json text.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="decode-1"></a>

###decode/1##




<pre>decode(JsonText::<a href="#type-text">text()</a>) -> {ok, <a href="#type-value">value()</a>} | {error, <a href="#type-decode_error">decode_error()</a>}</pre>
<br></br>




Equivalent to [`decode(JsonText, [])`](#decode-2).

decode json text into erlang term.<a name="decode-2"></a>

###decode/2##




<pre>decode(JsonText::<a href="#type-text">text()</a>, Options::<a href="#type-decode_options">decode_options()</a>) -> {ok, <a href="#type-value">value()</a>} | {error, <a href="#type-decode_error">decode_error()</a>}</pre>
<br></br>






decode json text into erlang term.



Followins options are acceptable::




<dt id="allow_comments">allow_comments</dt>




<dt id="allow_comments.2">{allow_comments, boolean()}</dt>




<dd>
Allow JavaScript style comment in json text.
Default is false.
</dd>




<dt id="key_decode">{key_decode, existent_atom | binary | [existent_atom | binary | ignore]}</dt>




<dd>
Type of decoded key of json object.
Valid parameter is one of followings::
binary, existent_atom, [existent_atom, binary].
Default is binary.
</dd>


<a name="encode-1"></a>

###encode/1##




<pre>encode(JsonTerm::<a href="#type-value">value()</a>) -> {ok, <a href="#type-binary_text">binary_text()</a>} | {error, <a href="#type-encode_error">encode_error()</a>}</pre>
<br></br>




encode erlang term into json text.