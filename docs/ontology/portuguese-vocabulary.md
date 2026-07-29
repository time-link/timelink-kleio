# Portuguese Vocabulary

The Portuguese vocabulary is defined by **extending** the base ontology
(see [Base ontology](base-ontology.md)). It is assembled from several
files under `kleio-home/structures/`:

| File | Contents |
| --- | --- |
| [`pt-elements.yaml`](../../tests/kleio-home/structures/pt-elements.yaml) | Portuguese aliases for the core elements (`dia`, `mes`, `ano`, `data`, `nome`, …). |
| [`pt-groups.yaml`](../../tests/kleio-home/structures/pt-groups.yaml) | Portuguese top-level groups (`fonte`, `pt-acto`, `acto`, `evento`, `item`, …) and overrides. |
| [`pt-actorm.yaml`](../../tests/kleio-home/structures/pt-actorm.yaml) | Male actors and the generic `n$` person. |
| [`pt-actorf.yaml`](../../tests/kleio-home/structures/pt-actorf.yaml) | Female actors. |
| [`pt-parentem.yaml`](../../tests/kleio-home/structures/pt-parentem.yaml) | Male relatives (kin) of an actor. |
| [`pt-parentef.yaml`](../../tests/kleio-home/structures/pt-parentef.yaml) | Female relatives (kin) of an actor. |
| [`pt-acts.yaml`](../../tests/kleio-home/structures/pt-acts.yaml) | Portuguese historical acts (baptism, marriage, death, notarial, …). |

## How the Portuguese vocabulary extends the base

There are three extension mechanisms, all using the `source:` key:

1. **Element aliases.** Portuguese elements specialize the base elements so
   that the same processing behaviour is triggered. Examples:

   ```yaml
   - element: {name: dia,  source: day,   description: "Sinónimo de day"}
   - element: {name: mes,  source: month, description: "Sinónimo de month"}
   - element: {name: ano,  source: year,  description: "Sinónimo de year"}
   - element: {name: data, source: date,  description: "Sinónimo de date"}
   - element: {name: nome, source: name}
   - element: {name: sexo, source: sex}
   ```

   This lets a Portuguese source write `dia`, `mes`, `ano`, `data`,
   `nome`, `tipo`, `valor`, `local`, `cota`, `fol`, `sumario`, `obs`,
   `mesmo_que`, etc.

2. **Group specialization.** Portuguese groups extend the base groups and
   override the positional elements to use the Portuguese element names.
   For example, `pt-acto` extends `historical-act` but uses
   `position: [id, dia, mes, ano]` instead of `[id, type, date]`:

   ```yaml
   - group:
       name: pt-acto
       source: historical-act
       position: [id, dia, mes, ano]
       guaranteed: [id, dia, mes, ano]
       contains: [actorm, actorf, item]
   ```

3. **Overrides.** A Portuguese group with the same name as a base group
   replaces it. For instance `rel` is redefined so that the date element
   is `data` instead of `date`:

   > "Short hand for relation Portuguese Override to have `data=` instead
   > of `date=`"

   ```yaml
   - group:
       name: rel
       source: relation
       position: [type, value, destname, destination, data]
   ```

---

## Top-level Portuguese groups

### `fonte` — the Portuguese source

> Este constitui o grupo principal para os documentos históricos
> portugueses. Admite qualquer dos grupos que tenham "acto" ou "evento"
> como fonte.

- **source**: `historical-source`
- **position**: `[id]`
- **also**: `tipo`, `data`, `ano`, `localizacao`, `loc`, `ref`, `obs`, `substitui`

> **Note on the second positional field.** The legacy `gacto2.str` defined
> a second positional field `tipo` after `id`, but different sources used
> different fields (`data`, `ano`, `loc`). This structure defines **no**
> second positional field — elements after `id` must be named explicitly.
> The field `ano` should be avoided in favour of `data`, which accepts
> year-only dates without padding.

`fonted` is a variant of `fonte` where the second positional field is
`data` rather than `tipo` (`position: [id, data, tipo]`).

### `pt-acto` — the Portuguese act (abstract)

> A Portuguese act. Abstract class.

- **source**: `historical-act`
- **position**: `[id, dia, mes, ano]`
- **guaranteed**: `[id, dia, mes, ano]`
- **contains**: `actorm`, `actorf`, `item`

All concrete Portuguese acts extend `pt-acto`.

### `acto` — generic act

> Actos genéricos, servem para vereações etc. Têm id, tipo, dia, mes, ano,
> loc e obs. Distinguem-se de `pt-acto` por terem o tipo como obrigatório.
> Compostos por items e por pessoas.

Example:

```
acto$v1793-2/juramento de posse dos almotaces/1/3/1793/f.18v/para os meses de março e abril
    presente$francisco quaresma
        ls$titulo/bacharel
        ls$cargo/almotace/obs=março e abril
```

### `evento` — non-formal event

> Algo que aconteceu, mas que não é um acto formal, por exemplo uma
> descrição de um evento numa carta.

- **source**: `pt-acto`; **position**: `[description, dia, mes, ano]`
- Has a very large `part` list of actor roles (beneficiário, cobrador,
  dador, destinatário, pagador, tomador, vendedor, comprador, …).

### `fim` — end of an act

> Fim de um acto ou de parte de um acto que pode ser processada para
> inferir valores, por exemplo o fim de um fogo num longo rol de
> confessados.

- **source**: `end`

### Other top-level Portuguese groups

| Group | Source | Description |
| --- | --- | --- |
| `item` | `object` | Parte de um acto, por exemplo um assunto numa acta. |
| `topico` | `topic` | Um tópico / assunto. |
| `lugar` | `geoentity` | Um lugar. |
| `bem` | `object` | Um bem móvel ou imóvel (edifício, terreno, objecto). |
| `fogo` | `object` | Um fogo / agregado familiar (household); `n$` é a cabeça do agregado. Contains `n`, `criado`, `criada`, `engeitado/a`, `escravo/a`, `referido/a`, `fim`. |
| `viagem` | `pevent` | Viagem, enquanto evento pessoal. |
| `viajante` | `person` | Uma pessoa que viaja. |
| `pevento` | `pevent` | Um evento pessoal (viagem, casamento, baptismo…). |
| `estadia` | `pevento` | Um evento de estadia num local. |
| `familia` | `object` | Uma família em genealogias. |
| `linha` | `object` | Uma linha de uma família em genealogias. |
| `geodesc` | `pt-acto` | Override of the base `geodesc`, using `dia/mes/ano` and `pt-acto`. |

---

## Portuguese elements

The Portuguese elements (from `pt-elements.yaml`) specialize the base
elements. Highlights:

| Portuguese | Base | Note |
| --- | --- | --- |
| `dia`, `dian` | `day` | `dian` = dia normalizado. |
| `mes`, `mesn` | `month` | `mesn` = mês normalizado. |
| `ano`, `anon` | `year` | `anon` = ano normalizado. |
| `data` | `date` | |
| `tipo` | `type` | |
| `valor` | `value` | |
| `localizacao`, `local` | `loc` | |
| `cota` | `ref` | Referência de documento em arquivo/biblioteca. |
| `fol`, `fols`, `folio`, `folios` | `ref` / `page` / `pages` | Fólios em documentos manuscritos. |
| `nome` | `name` | |
| `sexo` | `sex` | |
| `nomedest` | `destname` | Nome do destino de uma relação. |
| `iddest` | `destination` | Id do destino de uma relação. |
| `mesmo_que` | `same_as` | Id de outra ocorrência da mesma entidade no mesmo ficheiro. |
| `xmesmo_que` | `xsame_as` | Outra ocorrência desta entidade noutro ficheiro. |
| `sumario`, `sumário`, `resumo` | `summary` | Resumo de um documento. |
| `descricao`, `desc` | `sumario` → `summary` | Descrição do conteúdo de um elemento. |
| `substitui` | `replaces` | Fonte corrente substitui a fonte com este id. |
| `titulo` | `title` | Título de uma obra. |
| `pagina`, `paginas` | `page` / `pages` | |

---

## Portuguese acts

> 💡 **Looking for real examples?** See
> [Portuguese acts — examples](portuguese-examples.md) for short, annotated
> excerpts from `reference_sources` (baptism, marriage, death, crisma,
> rolls, vereação, misericórdia, devassa, escritura). Not every act below
> has an example — only those with a clean, representative source file.

All concrete Portuguese acts extend `pt-acto` (or another act). They are
defined in [`pt-acts.yaml`](../../tests/kleio-home/structures/pt-acts.yaml).
Below is the catalogue, grouped by domain. Positional fields are shown where
the act overrides them; otherwise the inherited `pt-acto` positions
(`id, dia, mes, ano`) apply.

### Parish records (sacraments)

| Act | Source | Description / positional fields |
| --- | --- | --- |
| `bap` | `pt-acto` | Baptismos (variante usada nos registos da Lousa por Rosário Campos e em Óbidos). Position `[id, dia, mes, ano, fol, loc, celebrante]`. Contains `celebrante`, `n`, `test`, `referido/a`, `procurador-pad`, `procurador-mad`. |
| `b` | `pt-acto` | Baptismos, versão original usada em Soure. Position `[id, dia, mes, ano, dian, mesn, anon, celebrante]`. |
| `cas` | `pt-acto` | Casamentos. Position `[id, dia, mes, ano, fol, loc, celebrante]`. Contains the full bride/groom and in-laws kinship tree (see [Kinship tree](#kinship-tree-in-marriage-acts)). |
| `termo` | `pt-acto` | Termo de casamento (same kinship tree as `cas`, declared as `arbitrary`). |
| `obito` | `pt-acto` | Óbitos. Position `[id, dia, mes, ano, fol, celebrante]`. Contains `celebrante`, `n`, `marido`, `mulher`, `pai`, `mae`, `referido/a`, plus group-elements `sacr`, `testamento`, `testamenteiro`, `locs`, `locf`, `causa`, `oficios`. |
| `o` | `obito` | Óbitos, estilo usado em Soure, sem o id do óbito (id opcional). Position `[dia, mes, ano]`. |
| `crisma` | `pt-acto` | Crismas. |
| `rol` | `pt-acto` | Róis (e.g. róis de confessados). Contains `fogo`. |

**Óbito group-elements** (each sources `group-element`, position `[value]`):

| Group | idprefix | Description |
| --- | --- | --- |
| `oficios` | `ofs` | Ofícios em registos de óbito. |
| `sacr` | `sac` | Sacramentos em registos de óbito. |
| `testamento` | `tst` | Testamento. |
| `locs` | `lcs` | Local de sepultura. |
| `locf` | `lcf` | Local de falecimento. |
| `causa` | `cau` | Causa de falecimento. |

### Notarial and legal acts

| Act | Source | Description |
| --- | --- | --- |
| `escritura` | `pt-acto` | Uma escritura notarial. Position `[id, dia, mes, ano, tipo]`. Has a very large `contains` list of actor roles (see [Actors](#portuguese-actors)). |
| `cartaperdao` | `pt-acto` | Carta de perdão. Position `[id, dia, mes, ano]`; also `cota`, `local`, `tipo`, `tabeliao`, `obs`. Contains `perdoante`, `perdoante-m/f`, `perdoado/a`, `vitima`, `vitima-m/f`, `crime`, `perdao`, `test`, `referido/a`, `tabeliao`. |
| `carta` | `pt-acto` | Carta. Position `[titulo, dia, mes, ano, ref]`. Contains `celebrante`, `actorm/f`, `object`, `abstraction`, `ls`, `rel`, `receptor`, `emissor`, `evento`, `topico`, `emissor-representante`. |
| `adenda` | `pt-acto` | Adenda a uma carta/escritura. |
| `docregio` | `pt-acto` | Documento régio. |
| `docpontificio` | `docregio` | Documento pontifício. |
| `docepiscopal` | `docregio` | Documento episcopal. |
| `chanc` | `pt-acto` | Chancelarias régias. Position `[id, dia, mes, ano, fol]`. |
| `encarte` | `chanc` | Encartes. |
| `siza` | `object` | Registo de pagamento de sisa/siza (sécs. XVI–XVII). Position `[valor, juiz, escrivao, depositario, data]`. |
| `garantia` | `bem` | Garantia (bem). |
| `divida` | `object` | Dívida. Guaranteed `[valor, juro]`; also `prazo`, `moeda`, `valorn`. |
| `aforamento` | `bem` | Aforamento. |
| `prazo` | `aforamento` | Prazo (aforamento). |
| `lc` | `pt-acto` | Letra de câmbio (projecto DyncoopNet). Position `[id, dia, mes, ano, loc]`. Large `contains` list of commercial actors (beneficiário, cobrador, dador, destinatário, pagador, recebedor, tomador, endossante/ado, debitado, remetente…). |

### Government, municipal and ecclesiastical administration

| Act | Source | Description |
| --- | --- | --- |
| `vereacao` | `pt-acto` | Actas de vereação. |
| `pauta` | `pt-acto` | Pauta (de vereação). |
| `eleicao` | `pt-acto` | Eleição. |
| `juramento` | `pt-acto` | Juramento (de posse). |
| `amz` | `pt-acto` | Assentos da mesa numa misericórdia. Position `[id, dia, mes, ano, fol]`; also `sub`, `esmola`, `resumo`. Contains `eleito`, `eleitor`, `referido`. |
| `nom` | `pt-acto` | Nomeações de irmãos numa misericórdia. Position `[id, dia, mes, ano, fol, subs, esmola]`. |
| `beneficio` | `pt-acto` | Benefícios colações numa colegiada. |
| `gov` | `pt-acto` | Governos (com um extenso `part` de pastas ministeriais — `pm`, `minAdj`, `secEst…`, etc.). |
| `lista` | `pt-acto` | Lista genérica de pessoas. |
| `ordenancas` | `lista` | Ordenanças. |
| `milicias` | `ordenancas` | Milícias. |
| `credito` | `lista` | Lista de crédito (credor/a, devedor/a, procurador/a). |
| `rmerce` / `merce` | `pt-acto` / `rmerce` | Mercês. |
| `let` / `lbach` | `pt-acto` / `let` | Letras / letrados bacharéis. |
| `bio` | `lista` | Descrição biográfica (de dicionários biográficos). |

### Religious inquiries and qualifications

| Act | Source | Description |
| --- | --- | --- |
| `devassa` | `pt-acto` | Devassas temporais nas visitas pastorais. Inclui testemunhas (`testo`/`testa`), que contêm acusações (`acus`) de casos (`caso`). Position `[id, dia, mes, ano]`; also `folio`, `fol`, `visdor`, `paroco`, `obs`. |
| `acus` | `object` | Acusações em devassas/inquéritos. Position `[idcaso, obs]`; contains `caso`. |
| `caso` | `object` | Um caso numa devassa ou inquérito. Position `[id, tipo]`; contains `acusado/a`, `referido/a`. |
| `denuncia` | `pt-acto` | Denúncia de um caso (cadernos dos promotores da Inquisição). |
| `hab` | `pt-acto` | Habilitações da Ordem de Cristo e Santo Ofício. Position `[id, cota, data]`. |
| `po` | `hab` | (Variante de habilitação.) |
| `proc` | `pt-acto` | Processos de ordenação sacerdotal (usado em Soure). Position `[id, cota, data]`. |

### Other acts

| Act | Source | Description |
| --- | --- | --- |
| `memoria58` | `pt-acto` | Formato para registo das memórias paroquiais de 1758. Contains `freguesia`. |
| `apontamentos` / `apontamentosd` | `memoria58` | Sinónimo de `memoria58`. |
| `automed` | `pt-acto` | Auto de medição (tombos) — medição de extremas de uma terra. Contains `terra`. |
| `matricula` | `pt-acto` | Matrícula de doentes no Hospital das Caldas da Rainha. Contains `enfermo/a`, `referido/a`. |
| `pas` | `pt-acto` | Passagem, registo de viagens (Lia Nunes). Position `[id, dia, mes, ano, fol, local, destino]`; contains `mestre`, `n`, `referido/a`. |
| `wviagem` | `pt-acto` | Viagem (por água/barco). Position `[id, dia, mes, ano, barco]`. |
| `arrolamento` | `pt-acto` | Arrolamento. |
| `capela` | `pt-acto` | Extinção de capelas (chancelarias régias de D. José). |
| `ar` | `pt-acto` | (Contém `proparr`, `bemarr`, `fornarr`, `rendarr`.) |
| `integr` | `pt-acto` | (Contém `referido/a`, `acionista/f`, `ls`, `rel`, `acao`.) |
| `lcc` | `pt-acto` | (Contém `n`.) |

### Geo/administrative sub-groups (used inside acts like `memoria58`)

These are geoentities / abstractions / objects placed inside acts:

| Group | Source | |
| --- | --- | --- |
| `freguesia` | `geoentity` | Freguesia (memórias paroquiais de 1758). |
| `provincia`, `bispado`, `comarca`, `termog`, `termoc`, `honra`, `gref`, `couto`, `igreja` | `geoentity` | |
| `irmandade`, `hospital`, `confraria`, `misericordia`, `idonatario` | `abstraction` | |
| `convento`, `mosteiro` | `object` | |
| `pdonatario` | `pt-actorm` | |

### Kinship tree in marriage acts

The `cas` and `termo` acts contain the complete kinship tree for bride and
groom, up to grandparents. The naming convention is:

- `p` = paternal, `m` = maternal; each letter prefixes the relative.
- `noivo` / `noiva` = groom / bride.
- `marido1..3` / `mulher1..3` = previous husbands / wives (numbered).
- `pai` / `mae` = father / mother; `ppai`, `mpai`, `pmae`, `mmae` =
  paternal/maternal grandfather/grandmother; and so on for
  great-grandparents (`pppai`, `mppai`, …).

For example, inside `noivo` you find `pnoivo` (father of the groom),
`mpnoivo` (mother of the groom), `ppnoivo` (paternal grandfather), etc.
The bride (`noiva`) similarly contains `pnoiva`, `mpnoiva`, … and the
groom's previous wives (`mulher1`, …). The full list of these kin groups
is defined in [`pt-parentem.yaml`](../../tests/kleio-home/structures/pt-parentem.yaml)
and [`pt-parentef.yaml`](../../tests/kleio-home/structures/pt-parentef.yaml).

---

## Portuguese actors

Portuguese actors are the people that appear inside acts. They are defined
in [`pt-actorm.yaml`](../../tests/kleio-home/structures/pt-actorm.yaml) and
[`pt-actorf.yaml`](../../tests/kleio-home/structures/pt-actorf.yaml). They
extend the base `actorm`/`actorf` (via the Portuguese intermediates
`pt-actorm`/`pt-actorf`) and use `position: [nome, sexo]`.

### Generic actor groups

| Group | Source | Description |
| --- | --- | --- |
| `pt-actor` | `actorm` | A Portuguese actor, gender set by the `sexo` element. |
| `pt-actorm` | `actorm` | A Portuguese **male** actor. |
| `pt-actorf` | `actorf` | A Portuguese **female** actor. |
| `n` | `pt-actorm` | Pessoa genérica ou principal actor de um acto (e.g. criança em baptismo). Contains `kin-m`, `kin-f`, `notkin-m`, `notkin-f`. |
| `referido` | `pt-notkin-m` | Pessoa **referida** num acto, não necessariamente presente (male). Cannot contain personal events, to avoid recursion. |
| `referida` | `pt-notkin-f` | Pessoa **referida** num acto (female). |
| `presente` | `pt-actorm` | Pessoa presente num acto, geralmente como testemunha. |
| `presente-f` | `pt-actorf` | Pessoa presente (female). |

> **Gender-ambiguous actors.** A small number of actors extend `pt-actor`
> (gender set by the element, not fixed): `perdoante`, `vitima`, and the
> test group `test`.

### Male actors

All extend `pt-actorm` (unless noted). Sourced from `pt-actorm.yaml`.

`abonador`, `acionista`, `acusado`, `agente_financeiro`, `agente_seguros`,
`apelado`, `apelante`, `apresentado`, `arrematante`, `arrendatario`,
`autor`, `autoridade`, `beneficiario`, `beneficiario1` … `beneficiario6`
(and their `-representante` / `-procurador` variants), `celebrante` (pessoa
que celebra num acto, e.g. o pároco num baptismo), `cheater`, `cobrador`,
`cobrador1` … `cobrador4` (and `cobrador-representante` …), `comerciante`,
`comprador`, `confirmante`, `constituinte`, `contraente`, `credor`,
`dador`, `dador1` … `dador4` (and `-representante`), `debitado`,
`defector`, `demandado`, `demandador`, `denunciante` (contains `acus`,
`caso`), `destinatario`, `destinatario1`, `destinatario2`, `devedor`,
`doador`, `donatario`, `dotado`, `dotante`, `eleito`, `eleitor`,
`emissor`, `emissor-representante`, `endossado`, `endossante`, `enfermo`,
`enfiteuta`, `escambador`, `escrivao`, `escravo`, `fiador`, `fonte_info`,
`foreiro`, `fretador`, `informador`, `inquirido`, `inquiridor`,
`intermediario`, `juiz`, `louvado`, `outorgante`, `padroeiro`, `pagador`,
`pagador1` … `pagador5` (and `-representante`), `perdoado`, `perdoante-m`,
`pm`, `presente`, `procurador`, `procurador-pad` (procurador do padrinho),
`proprietario`, `quitado`, `quitante`, `recebedor`, `recebedor1` …
`recebedor4` (and `-proc`), `receptor`, `remetente`, `rendeiro`,
`renunciante`, `representante`, `requerente`, `requerido`, `reu`, `rp`,
`rv`, `senhorio`, `subenfiteuta`, `tabeliao`, `testador`,
`testamentario`, `testamenteiro`, `testo` (contains `acus`, `acusa`,
`referido/a`), `tomador`, `tomador1` … `tomador5` (and `-representante`),
`transportador`, `vedor`, `vendedor`, `vitima-m`.

Additional non-actor / helper male groups: `arrem` (source `n`), `ts`
(source `n`).

### Female actors

All extend `pt-actorf` (unless noted). Sourced from `pt-actorf.yaml`.

`abonadora`, `acionistaf`, `acusada`, `apelantef`, `arrematantef`,
`arrendataria`, `autorf`, `beneficiaria`, `compradora`, `constituintef`,
`contraentef`, `credora`, `demandadora`, `destinataria`, `devedora`,
`doadora`, `donataria`, `dotada`, `dotantef`, `enferma`, `enfiteutaf`,
`escambadora`, `escrava`, `fiadora`, `foreira`, `inquirida`, `na`,
`outorgantef`, `perdoada`, `perdoante-f`, `presente-f`, `procuradora`,
`procurador-mad` (procurador da madrinha — note: defined in
`pt-actorm.yaml` but sources `pt-actorf`), `quitada`, `quitantef`,
`renunciantef`, `reuf`, `senhoria`, `subenfiteutaf`, `testa` (contains
`acus`, `acusa`, `referido/a`), `testadora`, `testamentaria`,
`testamenteira`, `vendedora`.

Non-kin female groups (source `pt-notkin-f`): `criada`, `engeitada`,
`referida`.

### Non-kin persons (servants, slaves, foundlings)

| Group | Source (PT) | Base |
| --- | --- | --- |
| `pt-notkin-m` | — | `notkin-m` (servant, slave, foundling, referred) |
| `pt-notkin-f` | — | `notkin-f` |
| `criado` | `pt-notkin-m` | male servant |
| `criada` | `pt-notkin-f` | female servant |
| `engeitado` / `enjeitado` | `pt-notkin-m` | male foundling |
| `engeitada` / `enjeitada` | `pt-notkin-f` | female foundling |
| `escravo` | `pt-actorm` | male slave |
| `escrava` | `pt-actorf` | female slave |

---

## Portuguese relatives (kin)

Relatives are the people related by kin to an actor inside an act. They
are defined in [`pt-parentem.yaml`](../../tests/kleio-home/structures/pt-parentem.yaml)
(male) and [`pt-parentef.yaml`](../../tests/kleio-home/structures/pt-parentef.yaml)
(female). They extend the base kin groups via the Portuguese intermediates
`pt-kin-father`, `pt-kin-mother`, `pt-kin-husband`, `pt-kin-wife`,
`pt-kin-son`, `pt-kin-daughter`.

### Core Portuguese kin groups

| Group | Source | Base | Description |
| --- | --- | --- | --- |
| `pt-kin-m` / `pt-kin-f` | — | `kin-m` / `kin-f` | A Portuguese male/female kin. |
| `pt-notkin-m` / `pt-notkin-f` | — | `notkin-m` / `notkin-f` | Non-kin. |
| `pt-kin-father` | — | `kin-father` | Father. |
| `pt-kin-mother` | — | `kin-mother` | Mother. |
| `pt-kin-husband` | — | `kin-husband` | Husband. |
| `pt-kin-wife` | — | `kin-wife` | Wife. |
| `pt-kin-son` | — | `kin-son` | Son. |
| `pt-kin-daughter` | — | `kin-daughter` | Daughter. |
| `parentem` | `kin-m` | | Generic male relative. |
| `parentef` | `pt-kin-f` | | Generic female relative. |

### Direct relatives

| Male | Female | Relation |
| --- | --- | --- |
| `pai` | `mae` | father / mother |
| `marido` | `mulher` | husband / wife |
| `marido1`..`marido3` | `mulher1`..`mulher3` | previous husbands / wives |
| `filho` | `filha` | son / daughter |
| `irmao` | `irma` | brother / sister |
| `noivo` | `noiva` | groom / bride |
| `neto` | `neta` | grandson / granddaughter |
| `afilhado` | `afilhada` | godson / goddaughter |
| `sogro` | `sogra` | father-in-law / mother-in-law |
| `sobrinho` | `sobrinha` | nephew / niece |
| `genro` | `nora` | son-in-law / daughter-in-law |
| `pad`, `pad1`..`pad4` | `mad`, `mad1`..`mad4` | godfather(s) / godmother(s) |

### Ancestors (grandparents and beyond)

The schema encodes ancestors using a prefix code, where each `p` (paternal)
or `m` (maternal) is added as you go up a generation. This is used heavily
inside the `cas`/`termo` marriage acts.

| Code prefix | Meaning |
| --- | --- |
| `p` | paternal side |
| `m` | maternal side |

Applied to `pai` (father), `mae` (mother), `marido` (husband), `mulher`
(wife), `noivo`/`noiva` (groom/bride), each relative is prefixed:

- `ppai` = paternal grandfather, `mpai` = maternal grandfather
- `pmae` = paternal grandmother, `mmae` = maternal grandmother
- `pppai`, `mppai`, `pppmae`, … = great-grandparents

The parents of the groom/bride use the relative's name as the stem, e.g.
`pnoivo`/`mpnoivo` = father/mother of the groom; `ppnoivo`/`mpnoivo` =
paternal/maternal grandfather of the groom. The same pattern applies to
`noiva`, `mulher1..3`, `marido1..3`. The complete enumerated set (down to
great-grandparents) is defined in the two parent YAML files.

### Godparents and their spouses

Inside `pad`/`mad` (godfather/godmother) the spouse is encoded as
`mrmad` (marido da madrinha — husband of the godmother). Variants exist
for numbered godparents: `pad1`..`pad4`, `mad1`..`mad4`, each with their
own `mrmad1`..`mrmad4`.
