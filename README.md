# Projetos — Conceitos Fundamentais de Linguagens de Programação (MAC0316)

Este repositório reúne implementações e exercícios práticos desenvolvidos durante a disciplina "Conceitos Fundamentais de Linguagens de Programação" (MAC0316).

Os trabalhos exploram a implementação de interpretadores em Racket (dialeto Scheme), enfatizando design de linguagens, estratégias de avaliação, gestão de ambientes/escopos e técnicas de otimização (como avaliação por demanda). Cada exercício foi pensado para exercitar raciocínio formal, depuração e engenharia de software aplicada a linguagens de programação.

Conteúdo:

- EP0 — Interpretador básico com if e booleanos
- EP1 — Novo interpretador com LET (variáveis e escopo)
- EP2 — Interpretador com avaliação por demanda (lazy / call-by-need)


## Tecnologias e conceitos usados

- Linguagem: Racket (Scheme)
- Paradigma: Programação funcional e metaprogramação
- Conceitos de linguagens: AST, ambientes (bindings), escopo léxico, avaliação estrita vs. não-estrita, thunks/memoização
- Boas práticas: modularidade, documentação do código, testes manuais e verificação por execução

---

## Projetos

Cada subdiretório contém os arquivos fonte (.rkt) entregues para a disciplina. Abaixo há uma descrição concisa do que cada projeto faz e quais competências ele demonstra.

### EP0 — Interpretador básico com if e booleanos

- Arquivo: `EP0 - Interpretador básico com if e booleanos/EP0.rkt`
- O que faz: um interpretador simples para uma linguagem de expressões que inclui valores booleanos e a expressão condicional `if`.
- Tópicos abordados: definição de uma AST simples, avaliação recursiva, tratamento de erros básicos (tipos e aridade), representação de valores e operações lógicas.
- Competências demonstradas: implementação de um avaliador/interpretador do zero, raciocínio recursivo sobre estruturas de dados, clareza na separação entre sintaxe e semântica.

### EP1 — Novo Interpretador com LET

- Arquivo: `EP1 - Novo Interpretador com LET/interpretador_EP1.rkt`
- O que faz: extensão do interpretador para suportar ligações locais via `let` (introduzindo noções de ambiente e escopo).
- Tópicos abordados: criação e manutenção de ambientes de execução, resolução de identificadores, escopo léxico e estratégias de substituição versus ambiente.
- Competências demonstradas: modelagem de tabelas de símbolos/ambientes, entendimento de escopos e bindings, e implementação de semânticas de variáveis de forma robusta.

### EP2 — Interpretador com Avaliação por Demanda

- Arquivos: `EP2 - Interpretador com Avaliação por Demanda/EP2-parte1.rkt` e `EP2 - Interpretador com Avaliação por Demanda/Ep2-parte2.rkt`
- O que faz: implementação de avaliação por demanda (lazy evaluation / call-by-need) para a linguagem de estudo, incluindo a construção de thunks e memoização dos resultados.
- Tópicos abordados: comparação entre estratégias de avaliação (estrita vs. não-estrita), implementação de thunks, memoização para evitar reavaliação e implicações de performance e corretude.
- Competências demonstradas: projeto de algoritmos de execução eficientes, análise de trade-offs entre corretude e performance e capacidade de implementar otimizações semânticas.

---

## Como executar os exemplos

Requisitos: ter o Racket instalado. Em sistemas Linux, Racket normalmente pode ser instalado via gerenciador de pacotes ou baixado em https://racket-lang.org.

Executar um dos arquivos de exemplo (a partir da raiz do repositório):

```bash
# executar EP0
racket "EP0 - Interpretador básico com if e booleanos/EP0.rkt"

# executar EP1
racket "EP1 - Novo Interpretador com LET/interpretador_EP1.rkt"

# executar EP2 (parte 1 e 2 conforme ordenação dos arquivos)
racket "EP2 - Interpretador com Avaliação por Demanda/EP2-parte1.rkt"
racket "EP2 - Interpretador com Avaliação por Demanda/Ep2-parte2.rkt"
```

Observação: dependendo da estrutura interna dos arquivos, alguns podem expor funções para serem testadas interativamente no REPL do Racket em vez de rodarem como programas autônomos; abra os arquivos para instruções de uso adicionais (comentários no topo dos arquivos normalmente indicam como testá-los).

