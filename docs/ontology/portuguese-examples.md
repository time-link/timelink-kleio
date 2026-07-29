# Portuguese acts — examples

Short, real examples of the most common Portuguese acts, drawn from the
`reference_sources` corpus. Not every act defined in the schema has an
example here — only those with a clean, representative source file.

Each example shows the `fonte$` header followed by one act, lightly trimmed
to the essential lines. Indentation reflects containment (see
[Containment hierarchy](containment-hierarchy.md)).

> **Note on the `kleio$` line.** The first line of a file points to the
> structure (here the legacy `gacto2.str` / `gacto.str`, equivalent to the
> current `sources-structure.yaml`) and may set `prefix`, `translations`,
> etc. Kleio files use the `.kleio` extension; `.cli` is the legacy
> extension still found in old projects.
>
> **Syntax highlighting.** The Kleio code blocks below are left plain
> (uncolored). Kleio is not yet registered in
> [Linguist](https://github.com/github-linguist/linguist), so `` ```kleio ``
> would also render as plain text on GitHub. A TextMate grammar already
> exists in the `timelink-vscode` extension
> (`syntaxes/Kleio.tmLanguage`).
>
> **TODO (later):** contribute that grammar to Linguist so that `` ```kleio ``
> and `.kleio` / `.cli` files are highlighted natively on GitHub. Once
> merged, switch these fences to `kleio`.

---

## Parish records (sacraments)

### `bap` — baptism

From `paroquiais/baptismos/bapt1714.cli`. Positional:
`id / dia / mes / ano / fol / loc / celebrante`. Contains `celebrante`,
`n` (the baptized), `pn`/`mn` (parents), `pad`/`mad` (godparents).

```
kleio$gacto2.str/prefix=lousa/translations=1
   fonte$bapt1714/loc=a.u.c./tipo=bapt/obs=freguesia de sao silvestre da lousa

      bap$b1714-1/6/1/1714/fl.117v./igreja de sao silvestre/joao lopes serra(padre)/obs=com licenca do padre manuel lopes serra

         celebrante$joao lopes serra/id=b1714-1-per1-2
            ls$profissao/padre

         n$filipa/f/id=b1714-1-per1
            pn$joao de almeida/id=b1714-1-per2
               ls$morada/vila da lousa
            mn$maria antonia/id=b1714-1-per3
            pad$joao de arruda? frazao/id=b1714-1-per4
            mad$escolastica monteira/id=b1714-1-per5
```

### `cas` — marriage

From `paroquiais/casamentos/cas1714-1722-com-celebrante.cli`. Positional:
`id / dia / mes / ano / fol / loc / celebrante`. Shows the bride/groom and
their parents (`pnoivo`/`mpnoivo`, `pnoiva`/`mpnoiva`).

```
kleio$gacto.str/translations=1
   fonte$cas1714-1722-com-celebrante/loc=localizacao-a.u.c./tipo=casamentos

      cas$c1714-1/8/2/1714/fl.142v./igreja de sao silvestre/manuel da fonseca(prior)

         celebrante$manuel da fonseca/id=c1714-1-per1

         noivo$pedro martins/id=c1714-1-per2
            pnoivo$pedro martins/id=c1714-1-per3
               ls$morto/antes
            mnoivo$antonia simoes/id=c1714-1-per4
               ls$morta/antes

         noiva$ana fernandes/id=c1714-1-per5
            pnoiva$joana fernandes/id=c1714-1-per6
            mnoiva$maria goncalves/id=c1714-1-per7
```

### `o` — death (Óbitos, Soure style)

From `paroquiais/obitos/ob1688.cli`. `o` is the variant of `obito`
without a compulsory act id (positional `dia / mes / ano`). Shows the
group-elements `sacr`, `locs`, `oficios` nested in the act, and `marido`
inside the deceased.

```
kleio$gacto2.str
   fonte$ob1688/tipo=reg paroquiais/data=1688

      o$1/1/1688/id=obitos 1688-his1

         n$antonia francisca/f/id=obitos 1688-his1-per1
            ls$ec/v
            marido$joao rodrigues/id=obitos 1688-his1-per1-per2
               ls$morto/antes

         locs$igreja de s.tiago

      o$1/1/1688/id=obitos 1688-his2

         n$simao rodrigues marnoto?/m/id=obitos 1688-his2-per1
            ls$residencia/soure

         sacr$sim
         locs$adro
         oficios$1 de 9 nao tinha para mais
```

### `crisma` — confirmation

From `paroquiais/crisma/crisma1753.cli`. Each confirmed person `n` holds
`pai`/`mae` and `pad`/`mad`; the godfather's spouse is `mrmad` (marido da
madrinha).

```
kleio$gacto.str/prefix=quelfes
   fonte$crisma1753/data=17531228/tipo=crisma

      crisma$cr1753/28/12/1753

         n$joana/id=cr1753-per1
            pai$domingos gomes/id=cr1753-per1-per2
               ls$residencia/quelfes
            mae$maria de jesus/id=cr1753-per1-per3
            mad$juliana palermo/id=cr1753-per4
               mrmad$manuel rodrigues correia/id=cr1753-per5
```

---

## Household rolls

### `rol` + `fogo` — roll of confessants

From `roisdeconfessados/coja-rol-1841.cli`. A `rol` contains `fogo$
(households); each `fogo` is headed by `n$`, with spouse as `mulher$` and
servants as `criada$`. An `end$` would close each household so inference
fires independently (omitted in this excerpt). Note the use of `atr$`
instead of `ls$`.

```
kleio$gacto2.str/translations=1
   fonte$coja-rol-1841/data=1841/tipo=rol de confessados/loc=AUC III-Q GUIA 1 SECIII

      rol$rol1/28/9/1841

         fogo$fogo1/coja
            n$luciano jose pereira/m/id=fogo1-per7
               atr$profissao/padre
               atr$confessado/sim
               criada$luisa/f/id=fogo1-per8
                  atr$profissao/criada
                  atr$confessado/sim

         fogo$fogo2/coja
            n$joao de moura/m/id=fogo2-per10
               atr$confessado/sim
               mulher$maria/f/id=fogo2-per10-per11
                  atr$confessado/sim
```

---

## Municipal and charitable administration

### `acto` — generic act (vereaçães)

From `varia/vereacao.cli`. `acto` requires `tipo` as the second positional.
People present are registered as `presente$`, with kin (`pai`, `mae`,
`filho`, `filha`, `mulher`) nested inside.

```
kleio$gacto2.str/prefix=cbr
   fonte$vereacao/tipo=vereacoes/loc=AHMC/data=17930000%1793/ano=1793

      acto$v1793-1/vereacao/21/2/1793/f. 17

         presente$jose correia de melo/id=v1793-1-per1
            ls$cargo/preside a vereacao
            ls$cargo/juiz de fora (servindo de ....)
            ls$titulo/fidalgo da casa real
            pai$antonio de melo/id=v1793-1-per1-per1
            mae$maria correia/id=v1793-1-per1-per2
            filho$joaquim correira de melo/id=v1793-1-per1-per3

         presente$domingos de macedo/id=v1793-1-per2
            ls$cargo/escrivao da camara
```

### `amz` — misericórdia "mesa" assentos

From `varia/misMesa.cli`. Positional `id / dia / mes / ano / fol`; also
`resumo`. Contains `eleito$`, `eleitor$`, `referido$`.

```
kleio$gacto.str/translations=3
   fonte$misMesa/tipo=acordaos da mesa/data=1683:1723/localizacao=Arquivo da Santa Casa da Misericordia de Soure

      amz$amz1/3/10/1683/2/resumo=nomeacao de capelao que se fez na casa desta vila de soure

         referido$simao homem de oliveira/id=amz1-per1
            ls$cargo/capelao da missa quotidiana
            ls$morto/antes

         eleito$simao leao/id=amz1-per2
            ls$cargo/capelao da missa quotidiana
            ls$naturalidade/soure
```

### `nom` — nomeações de irmãos (misericórdia)

From `varia/nommiz.cli`. Contains `n$` (the appointed brother) and
`referido$`, with `rel$parentesco/...` relations.

```
kleio$gacto.str/translations=2
   fonte$nommiz/obs=Misericordia mis termos de aceitacao de Irmaos (1724-1829)

      nom$nom1a/3/7/1729/2

         n$leonardo de sa/id=nom1a-r1
            ls$titulo/irmao de maior condicao
            referido$diogo vaz de sa/id=nom1a-r1-per1
               ls$residencia/figueiro dos vinhos
               rel$parentesco/pai/leonardo de sa/nom1a-r1
```

---

## Religious inquiries

### `devassa` — pastoral visit inquiry

From `varia/dev1692.cli`. Contains witnesses `testo$` (male) / `testa$`
(female); each witness contains `acus$` (accusations) pointing to a
`caso$`, which contains the `acusado$`.

```
kleio$/translations=1
   fonte$dev1692/data=1692/tipo=devassa/ref=III-d,5,2,135/loc=auc

      devassa$devassa1692/10/4/1692/visdor=manuel joao/fol=fol?/obs=paroco luis alvaro pinto

         testo$jose machado/id=dev1692-per1
            ls$ec/s
            ls$residencia/soure
            ls$idade/24
            pai$manuel fernandes/id=dev1692-per1-per2
               ls$alcunha/ratinho

            acus$d1692-c1/literal=alcouceiro e alcoviteiro de varias mulheres/id=dev1692-per1-obj1

            caso$d1692-c1/alcouc/obs=dava mulheres em casa
               acusado$antonio cordeiro/id=d1692-2003
                  ls$profissao/sapateiro
                  ls$residencia/soure
```

---

## Notarial

### `escritura` — notarial deed (aforamento)

From `varia/auc_cartulario18.cli`. Positional
`id / dia / mes / ano / tipo / fol`. Shows the landlord (`senhoria$`),
the emphyteutae (`foreiro$` / `foreira$`), the `aforamento$` objects and
witnesses (`test$`).

```
kleio$gacto2.str
   fonte$auc_cartulario18/tipo=escrituras/loc=auc

      escritura$esc1517-e1/9/6/1517/aforamento em fatiosim/fol=3v-6/loc=grades/sumario=aforamento entre mosteiro de santa clara e joao esteves

         senhoria$margarida de meneses/id=esc1517-e1-per1
            ls$cargo/abadessa

         foreiro$joao esteves/id=esc1517-e1-per2
            ls$profissao/moleiro

         foreira$maria lopes/id=esc1517-e1-per3
            rel$parentesco/mulher/joao esteves/esc1517-e1-per2

         aforamento$moinho e levadas/id=esc1517-e1-obj2-3
            atr$foro/vinte alqueires de trigo

         test$pedro goncalves/m/id=esc1527-e1-per4
            ls$cargo/escudeiro do mosteiro
```

---

## Source files

| Act | Source file |
| --- | --- |
| `bap` | `sources/reference_sources/paroquiais/baptismos/bapt1714.cli` |
| `cas` | `sources/reference_sources/paroquiais/casamentos/cas1714-1722-com-celebrante.cli` |
| `o` | `sources/reference_sources/paroquiais/obitos/ob1688.cli` |
| `crisma` | `sources/reference_sources/paroquiais/crisma/crisma1753.cli` |
| `rol`/`fogo` | `sources/reference_sources/roisdeconfessados/coja-rol-1841.cli` |
| `acto` | `sources/reference_sources/varia/vereacao.cli` |
| `amz` | `sources/reference_sources/varia/misMesa.cli` |
| `nom` | `sources/reference_sources/varia/nommiz.cli` |
| `devassa` | `sources/reference_sources/varia/dev1692.cli` |
| `escritura` | `sources/reference_sources/varia/auc_cartulario18.cli` |

## See also

- [Portuguese vocabulary](portuguese-vocabulary.md) — full catalogue of acts, actors and kin.
- [Containment hierarchy](containment-hierarchy.md) — why these groups nest as they do.
