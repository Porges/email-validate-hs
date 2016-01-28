So you want to validate an email address...
---

This is a list of RFCs that are related to parsing and validating an email address. Yes, you will need all of them in some way or another. (And no, this library does not do everything correctly - it's lacking all the Unicode support.)

I've tried to avoid listing obsoleted RFCs, so for example we have 5322 instead of 2822 and 5321 instead of 2821.

### [RFC5322](https://tools.ietf.org/html/rfc5322): Internet Message Format

This is the main RFC that defines the email address syntax. If you want to do something basic (e.g., you want to write a self-referential regular expression) you should start here.

Among other fun things, you will learn that a C-style null-terminated string cannot hold an email address.

Don't look at [RFC5321](https://tools.ietf.org/html/rfc5321) for the syntax! It defines [similar-but-different](https://tools.ietf.org/html/rfc5321#section-4.1.2) versions of the ABNF (see below) rules. The rule names are case-sensitive!

### [RFC6532](https://tools.ietf.org/html/rfc6532): Internationalized Email Headers

This [extends the syntax](https://tools.ietf.org/html/rfc6532#section-3.2) from 5322 to handle Unicode in email addresses.

Don't read RFC6531! The situation is the same as with RFC5321.

### [RFC5234](https://tools.ietf.org/html/rfc5234): Augmented BNF for Syntax Specifications: ABNF

This describes the syntax that the syntax for email addresses is described by. It's fairly intuitive, but this also contains some basic rules used elsewhere (e.g. `VCHAR`) that aren't obvious, so you need to check them.

### [RFC3629](https://tools.ietf.org/html/rfc3629): UTF-8, a transformation format of ISO 10646

Okay, you probably don't need this as your language likely has facilities to do UTF reëncoding for you. It does have some ABNF definitions that are referenced in 6532, though.

### [RFC5198](https://tools.ietf.org/html/rfc5198): Unicode Format for Network Interchange

This falls into a similar category as the previous RFC, but I think Javascript doesn't get support until ES6 -- and you'll be supporting IE10 forever.

That said, this is a huge cop-out of an RFC. It's basically a thunk for [UAX#15](http://unicode.org/reports/tr15/), so go read that instead.

UAX#15 defines a family of Unicode "normalization" algorithms -- in simple terms, what it does is tell you how to convert "é" into "é", or vice versa.

Domain Names
===

The email address specification in 5322 is intentionally imprecise with regards to domain names. We can't have that.

### [RFC952](https://tools.ietf.org/html/rfc952): DOD INTERNET HOST TABLE SPECIFICATION

This defines the syntax of a domain name.

### [RFC1123](https://tools.ietf.org/html/rfc1123): Requirements for Internet Hosts -- Application and Support

This relaxes the syntax of a domain name to allow it to start with a digit. It also shows you how to write down or read 32-bit numbers (IPv4 addresses), so you can skip 780 or 790.

IP Literals
===

### [RFC4291](https://tools.ietf.org/html/rfc4291): IP Version 6 Addressing Architecture

This describes how to write down or read 128-bit numbers (IPv6 addresses).

### [RFC2765](https://tools.ietf.org/html/rfc2765): Stateless IP/ICMP Translation Algorithm (SIIT)

This describes how to write down or read 32-bit numbers as if they were 128-bit-numbers.

Limits
===

### [RFC1034](https://tools.ietf.org/html/rfc1034): DOMAIN NAMES - CONCEPTS AND FACILITIES
### [RFC1035](https://tools.ietf.org/html/rfc1035): DOMAIN NAMES - IMPLEMENTATION AND SPECIFICATION

You can tell these RFCs are old because they are shouty. Both of then define a few limits that are critical to getting your validation correct, but it doesn't matter which one you read.

In particular, labels are limited to 63 octets and names are limited to 255 octets (this includes 1 'length' byte per label plus one empty label (that still has a length byte), so the maximum length of a domain name is 253 characters when written in `the.usual.manner`).

### [RFC3696](https://tools.ietf.org/html/rfc3696): Application Techniques for Checking and Transformation of Names

This RFC tells you that an email address can be up to 320 octets long.

### [RFC5321](https://tools.ietf.org/html/rfc5321): Simple Mail Transfer Protocol

This RFC tells you that if you actually wish to send an email to an email address, it had better not be longer than 254 octets (see Erratum 1690 on RFC3696).

Unicode in Domain Names
===

### [RFC3492](https://tools.ietf.org/html/rfc3492): Punycode: A Bootstring encoding of Unicode for Internationalized Domain Names in Applications (IDNA)

The greatest algorithm name of all time? This converts Unicode to ASCII in a way that it can be reversed.

(Why do you need this? Because converting Unicode to ASCII makes the domain name longer, and an email address that is too long might be invalid - that simply won't do!)

### Aside: "I heard you like homographs"
> ### [RFC3490](https://tools.ietf.org/html/rfc3490): Internationalizing Domain Names in Applications (IDNA)

> Describes how to turn an ASCII domain name into a Unicode one and vice versa.

> ### [RFC3454](https://tools.ietf.org/html/rfc3454): Preparation of Internationalized Strings ("stringprep")

> This describes an algorithm but doesn't tell you how to use it. Luckily, you can then read:

> ### [RFC3491](https://tools.ietf.org/html/rfc3491): Nameprep: A Stringprep Profile for Internationalized Domain Names (IDN)

> This tells you how to use RFC3454.

Don't read any of these, as they've all been obsoleted now, mostly due to security concerns.

The implementation they describe is known as "IDNA2003". Instead, you need to read "IDNA2008". Of course, [IDNA2003 and IDNA2008 are incompatible](http://unicode.org/faq/idn.html), so two browsers might disagree on where a link goes. That doesn't matter here though, we only need to validate an email address. Ignore the fact that there's a middle-ground defined by [UTS#46](http://unicode.org/reports/tr46/).

The IDNA2008 specifications consist of:

### [RFC5890](https://tools.ietf.org/html/rfc5890): Internationalized Domain Names for Applications (IDNA): Definitions and Document Framework

This is like a glossary for the next 3 RFCs...

### [RFC5891](https://tools.ietf.org/html/rfc5891): Internationalized Domain Names in Applications (IDNA): Protocol

This replaces the `ToAscii`/`ToUnicode` algorithms of RFC3490.

### [RFC5892](https://tools.ietf.org/html/rfc5892): The Unicode Code Points and Internationalized Domain Names for Applications (IDNA)

70 pages of Unicode codepoints - hold me back! NB: **ERRATA EXIST**. Good luck with that.

### [RFC5893](https://tools.ietf.org/html/rfc5893): Right-to-Left Scripts for Internationalized Domain Names for Applications (IDNA)

Because you should put the hardest part of Unicode into a security-critical syntax designed for entry by people who can't program their microwaves.

### [RFC5894](https://tools.ietf.org/html/rfc5894): Internationalized Domain Names for Applications (IDNA): Background, Explanation, and Rationale

This RFC explains why it took 4 RFCs to define IDNA.

Meta
===

### [RFC2119](https://tools.ietf.org/html/rfc2119): Key words for use in RFCs to Indicate Requirement Levels

It is a rule that all RFCs published after RFC2119 SHOULD reference RFC2119, so you had better understand it.

### [RFC2606](https://tools.ietf.org/html/rfc2606): Reserved Top Level DNS Names

You should use these names in your test suite.


You might need these, I'm not sure
===

### [RFC7564](https://tools.ietf.org/html/rfc7564): PRECIS Framework: Preparation, Enforcement, and Comparison of Internationalized Strings in Application Protocols

I'm not sure if you need this yet. Anyhow, "stringprep" was a much more fun name.
