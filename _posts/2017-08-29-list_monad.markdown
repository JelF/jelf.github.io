---
layout: post
title:  Охуенная монада List
date:   2017-08-29 22:00:00 +0300
categories: ru haskell
---

{% highlight haskell %}
{% include 2017-08-29-list_monad/list-monad.hs %}
{% endhighlight %}

Вместо тысячи each

# Уровень влженности?

Кого это ебет. Сегодня по работе писал тройной цикл с гвардами (используя
`next if ...`, как завещал
[bbatsov](https://github.com/bbatsov/ruby-style-guide)), никакого сравнения
ни в красоте, ни, если уж на то пошло, в читаемости.

# Впрочем

{% highlight haskell %}
{% include 2017-08-29-list_monad/list-monad-no-do-notation.hs %}
{% endhighlight %}

Без do-нотации уже не то, выглядит как код, который пришлось писать на руби.
Жаль, в руби do-нотацию не внедрить нормально :(
