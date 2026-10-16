# Candidate workflowr profile

The declarative layout follows reflowR revision
`65fda310397220996bdf27135e7968b85144a6f0` and workflowr 1.7.2.
The workflowr CRAN source archive SHA-256 is
`1406ec1890396a2354b563c9eb555620f68abeb99eb7320b9880d119a4d277fe`.
Its CRAN index MD5 and 192 package member checksums were verified before review.

The renderer, receipt helper and page generator are maintained in cttiR. They
do not invoke reflowR's package-installing setup. The workflowr initializer was
used only in disposable review staging with Git actions and working-directory
changes disabled. Generated projects use declarative files, explicit rendering
and one root site. Scientific stages remain placeholders until implemented and
reviewed against their data contracts.

The profile is selectable explicitly through the public project API with a root
Git context. Explicit sync migration from the current generic bundle preserves
existing dependency revisions and edited manuscripts. Optional targets scheduling
checks content identities and reuses fresh study results. Runner
receipts verify content before and after computation, including externally bound
datasets. Descriptive tables are rendered only after freshness checks and use
HTML escaping. Results from earlier runs are not silently adopted.

The stage library derives from cttiR's immutable `standard-0.3.0` bundle. This
version fixes the empty predictor-list case in descriptive tidying; it preserves
the earlier bundle for existing project pins.

Upstream licenses: reflowR, MIT, copyright 2026 R. Heller; workflowr, MIT,
copyright 2016–2025 John Blischak. The following notice preserves their terms:

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
