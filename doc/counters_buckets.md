

# Module counters_buckets #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-bucket_bound">bucket_bound()</a> ###


<pre><code>
bucket_bound() = number() | infinity
</code></pre>




### <a name="type-buckets">buckets()</a> ###


<pre><code>
buckets() = [<a href="#type-bucket_bound">bucket_bound()</a>, ...]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bound-2">bound/2</a></td><td></td></tr><tr><td valign="top"><a href="#default-0">default/0</a></td><td>
Default buckets.</td></tr><tr><td valign="top"><a href="#exponential-3">exponential/3</a></td><td>
Creates <code>Count</code> buckets, where the lowest bucket has an
upper bound of <code>Start</code> and each following bucket's upper bound is <code>Factor</code>
times the previous bucket's upper bound.</td></tr><tr><td valign="top"><a href="#linear-3">linear/3</a></td><td>
Creates <code>Count</code> buckets, each <code>Width</code> wide, where the lowest
bucket has an upper bound of <code>Start</code>.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>
Buckets constructor.</td></tr><tr><td valign="top"><a href="#position-2">position/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bound-2"></a>

### bound/2 ###

`bound(L, Value) -> any()`

<a name="default-0"></a>

### default/0 ###

<pre><code>
default() -&gt; <a href="#type-buckets">buckets()</a>
</code></pre>
<br />

Default buckets.

```erlang

  1> counters_buckets:default().
  [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]
```

<a name="exponential-3"></a>

### exponential/3 ###

<pre><code>
exponential(Start::number(), Factor::number(), Count::pos_integer()) -&gt; <a href="#type-buckets">buckets()</a>
</code></pre>
<br />

Creates `Count` buckets, where the lowest bucket has an
upper bound of `Start` and each following bucket's upper bound is `Factor`
times the previous bucket's upper bound.

```erlang

  3> counters_buckets:exponential(100, 1.2, 3).
  [100, 120, 144]
```

The function raises `{invalid_value, Value, Message}` error if `Count`
isn't positive, if `Start` isn't positive, or if `Factor` is less than or
equals to 1.

<a name="linear-3"></a>

### linear/3 ###

<pre><code>
linear(Start::number(), Step::number(), Count::pos_integer()) -&gt; <a href="#type-buckets">buckets()</a>
</code></pre>
<br />

Creates `Count` buckets, each `Width` wide, where the lowest
bucket has an upper bound of `Start`.

```erlang

  2> counters_buckets:linear(10, 5, 6).
  [10, 15, 20, 25, 30, 35]
```

The function raises `{invalid_value, Value, Message}` error if `Count`
is zero or negative.

<a name="new-0"></a>

### new/0 ###

`new() -> any()`

<a name="new-1"></a>

### new/1 ###

`new(RawBuckets) -> any()`

Buckets constructor

<a name="position-2"></a>

### position/2 ###

`position(Buckets, Value) -> any()`

