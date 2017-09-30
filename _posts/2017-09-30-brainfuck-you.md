---
layout: post
title:  Делаем Brainfuck-машину своими руками
date:   2017-09-30 16:20
categories: ru postgresql
---

# [Brainfuck](https://ru.wikipedia.org/wiki/Brainfuck)
— один из самых простых
<span class="simplification" title="На самом деле нет, потому что в стандарте brainfuck
заложено ограничение на память, а лента тьюринга, сука, бесконечная">
тьюринг-полных </span> языков. После него, наверно, следует lisp и сочувствующие

Простота реализации brainfuck позволяет наглядно продемонстрировать понятие
полноты по тюьрингу. Например, CTE в postgresql

{% highlight sql %}
{% include 2017-09-30-brainfuck-you/brainfuck.sql %}
{% endhighlight %}

{% highlight shell %}
$ psql < brainfuck.sql

                  output                  | step
------------------------------------------+------
 Hello World! Brainfuck and psql are cool |  518
 (1 строка)
{% endhighlight %}

Охуенно же, разве нет?

# PS

Можно конечно добавить парочку хитрожопых оптимизаций, чтобы лучше работало,
но основная проблема в операциях над памятью (в коде `tbl`)
