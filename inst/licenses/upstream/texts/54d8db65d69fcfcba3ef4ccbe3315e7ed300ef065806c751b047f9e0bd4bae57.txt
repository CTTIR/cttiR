# Trademarks, affiliation and acknowledgement

## harbouR is an independent, third-party client

harbouR is developed and published independently by its authors. It is
**not** affiliated with, endorsed by, sponsored by, certified by, or in
any way officially connected to SeaTable GmbH.

No statement in this package, its documentation, or its website should be
read as suggesting that SeaTable GmbH has reviewed, approved, supported or
warranted harbouR. Requests for support with harbouR belong at
<https://github.com/CTTIR/harbouR/issues>, not with SeaTable GmbH.

## Trademarks

SeaTable is a trademark of

> SeaTable GmbH
> 117er Ehrenhof 5, 55118 Mainz, Germany
> Amtsgericht Mainz, HRB 49723

All trademarks, service marks, trade names and product names referred to
in this package are the property of their respective owners.

harbouR uses the name "SeaTable" solely to describe, truthfully and
accurately, the service that this software communicates with. That is
nominative use: the name identifies SeaTable GmbH's product, it is used no
more than is necessary to do so, and nothing about the use suggests
sponsorship or endorsement. harbouR claims no right, title or interest in
the mark.

harbouR does not distribute, bundle, or reproduce any logo, wordmark,
icon, colour scheme or other brand asset belonging to SeaTable GmbH. The
harbouR logo is an original work by the package's authors.

## What harbouR is for

harbouR is written for two things:

1. **SeaTable data files.** Reading and writing a `.dtable` export
   directly, with no server involved. This needs nothing but the file.
2. **A SeaTable Server you run yourself.** Talking to a self-hosted
   instance over its REST API, using credentials your own installation
   issues.

That is the intended scope, and it is what the documentation, the
examples and the tests describe. harbouR is a tool for people who
administer their own SeaTable deployment, or who simply have an exported
file and want to analyse it in R.

harbouR takes a server URL as an argument and makes no judgement about
which host you point it at; nothing here should be read as a technical
restriction, only as a statement of what the package is built and
documented for.

## Relationship to the software

harbouR is a client library. It contains no SeaTable server code and
redistributes no SeaTable software. It communicates with a SeaTable server
exclusively through the public REST API that SeaTable GmbH documents at
<https://api.seatable.com/> and provides for third-party applications.

Using harbouR does not grant you any right to use SeaTable. Your use of
SeaTable, however you obtain and run it, is governed entirely by your own
agreement with SeaTable GmbH or Seafile Ltd. and by their licence terms,
and is a matter between you and them. harbouR neither modifies nor
supplements those terms. In particular, it is your responsibility to
ensure that your SeaTable Server edition and licence permit the use you
put it to.

harbouR is licensed under the MIT licence and is provided **without
warranty of any kind**; see `LICENSE`. That disclaimer covers harbouR
only. It says nothing about the SeaTable service.

## Acknowledgement

harbouR is a client. Everything it does rests on work SeaTable GmbH and
Seafile Ltd. did first, and on a series of decisions they made about how
open that work would be. Those decisions are the reason an independent
client like this one could be written at all, so they are worth naming.

**They publish the API contract itself, not just a description of it.**
The OpenAPI 3.0 specification is a public repository
([seatable/openapi](https://github.com/seatable/openapi)) covering 387
operations, and it is the source the reference documentation, the PHP
client and the tooling are generated from. It is kept on one branch per
server version, from v4.0 to v6.2, so the contract a given release
promised stays available after that release is gone. Their CI refuses a
specification whose operations lack a summary, a description, a tag or a
declared security scheme.

**The documentation is open to everyone.** The full reference at
<https://api.seatable.com/> reads without an account, a registration or a
payment. The admin and developer manuals are public repositories that
invite pull requests. They also publish `llms.txt` and `llms-full.txt`,
so the documentation is legible to machines as well as people.

**They write down what they break.** SeaTable keeps a public API
changelog by server version, and it records removals as plainly as
additions: `/dtable-server` and `/dtable-db` were marked deprecated in
5.2 and removed in 5.3, with dates. harbouR was pointed at exactly those
endpoints. Without that changelog the package would still be aimed at an
API that no longer exists — this is not a hypothetical benefit.

**They support people building on top.** Official client libraries for
Python and PHP are Apache-2.0, as are the `dtable-sdk` and the HTML-page
SDK; the n8n integration nodes are MIT. The community forum has a
Developer Talk category for API questions, and the API reference points
there for help.

**They let you run it yourself.** SeaTable Server can be self-hosted, and
the Developer Edition needs no licence key. It is described as being for
"developers and small teams using SeaTable as a database backend via APIs
and scripts"
([SeaTable admin manual](https://admin.seatable.com/introduction/editions/)), which is precisely the use case harbouR serves, and on your
own server you decide which API limits apply, or whether any apply at all.
harbouR is written for that deployment, and for the exported `.dtable`
files it produces.

A note on accuracy, because an acknowledgement should not overstate:
SeaTable Server is **not** open source. Its own admin manual lists
per-component licences, and the component that holds the tables,
`dtable-server`, is proprietary. What is published under a recognised
open licence are the official client libraries and SDKs (Apache-2.0) and
the n8n integration nodes (MIT).

The OpenAPI specification and the reference documentation are published
publicly and readable without registration, but they carry no open
licence — and harbouR does not rely on one. It reproduces no
documentation prose. What it uses are endpoint paths, HTTP verbs and
JSON field names, and it relies on Article 1(2) of Directive
2009/24/EC, under which "ideas and principles which underlie any
element of a computer program, including those which underlie its
interfaces, are not protected by copyright". In *SAS Institute v World
Programming* (C-406/10) the CJEU held that neither the functionality of
a program, nor its programming language, nor the format of its data
files constitutes a form of expression protected as a computer program.
Germany transposes the same exclusion in § 69a(2) UrhG.

Two limits on that, stated because they are real. The Court reserved
the possibility (C-406/10, para. 45) that a data file format might
attract ordinary copyright as a work if it is its author's own
intellectual creation. And § 69g(1) UrhG preserves other regimes
entirely — unfair competition, trade secrets, trademarks and contract
are untouched by the copyright exclusion.

SeaTable's own terms point the same way. The dtable-server EULA defines
applications that talk to the Server through its API as "third-party
software" and says of them: "The provisions of the EULA do not apply to
any such third-party software."
([source](https://admin.seatable.com/introduction/dtable-server-license/))
harbouR is such an application, and the EULA's restrictions — including
the one on reverse engineering, which is directed at the Server's
source code — are not written to reach it.

None of the above is legal advice, and none of it was written by a
lawyer. It is the reasoning harbouR's authors relied on, with the
sources named so that anyone can check it or take their own advice.

SeaTable is developed by Seafile Ltd. (Beijing), which grew it out of the
Seafile project; SeaTable GmbH
(Mainz), founded in July 2020 by Dr. Ralf Dyllick-Brenzinger and
Christoph Dyllick-Brenzinger, is the German company behind it.
Thanks are owed to both.

If SeaTable is useful to you, support the people who build it:
<https://seatable.com/>.
