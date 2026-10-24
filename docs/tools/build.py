from pathlib import Path
import json,re,html
R=Path(__file__).resolve().parents[1]
cs=[json.loads(p.read_text()) for p in sorted((R/'capitulos').glob('*.json'))]
def esc(x):return html.escape(x)
def layout(title,body,current='',toc=''):
 nav='<a href="index.html" class="home">Da pergunta ao artigo<span>Modelagem de distribuição de espécies com R</span></a><label for="search">Buscar no livro</label><input id="search" type="search" placeholder="Conceito, função, capítulo…"><div id="search-results" aria-live="polite"></div><nav aria-label="Capítulos"><a href="index.html">Apresentação</a>'+''.join(f'<a {"aria-current=page" if c["slug"]==current else ""} href="{c["slug"]}.html"><small>{i:02d}</small>{c["title"]}</a>' for i,c in enumerate(cs,1))+'</nav>'
 return f'''<!doctype html><html lang="pt-BR"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><meta name="description" content="Livro do Curso SDM 2026: uma história de pesquisa, conceitos, ODMAP e exemplos de R."><title>{esc(title)} · Curso SDM 2026</title><link rel="stylesheet" href="assets/book.css"><script src="assets/search-index.js" defer></script><script src="assets/book.js" defer></script></head><body><a class="skip" href="#main">Ir para o conteúdo</a><header class="mobile"><button id="menu" aria-expanded="false" aria-controls="sidebar">☰ Sumário</button><span>Curso SDM 2026</span></header><aside id="sidebar">{nav}</aside><div class="reader"><div class="topbar"><span>CURSO SDM 2026 · eNichos</span><div><button id="theme" type="button">Alternar tema</button><button id="print" type="button">Imprimir</button></div></div><main id="main">{body}<footer>George Amaro · <a href="mailto:george.amaro@embrapa.br">george.amaro@embrapa.br</a><br>Curso SDM 2026 · <a href="18-fontes-e-reproducao.html#licenca">CC BY-NC 4.0</a> · <a href="18-fontes-e-reproducao.html">Fontes e reprodução</a></footer></main><aside class="page-toc" aria-label="Nesta página"><b>Nesta página</b>{toc}</aside></div><div id="status" role="status" aria-live="polite"></div></body></html>'''
index=[]
for i,c in enumerate(cs):
 heads=[]
 def heading(m):
  n=len(heads)+1; anchor='licenca' if m.group(1)=='Licença' else f'sec-{n}'; heads.append((anchor,m.group(1))); return f'<h2 id="{anchor}">{m.group(1)}</h2>'
 body=re.sub(r'<h2>(.*?)</h2>',heading,c['body'])
 body=f'<p class="eyebrow">CAPÍTULO {i+1:02d} · {c["stage"]}</p><h1>{c["title"]}</h1><p class="lead">{c["lead"]}</p><p class="source">Base no curso: slides {c["slides"]}.</p>'+body
 prev=cs[i-1] if i else None; nxt=cs[i+1] if i+1<len(cs) else None
 body+='<nav class="pager" aria-label="Navegação entre capítulos">'+(f'<a href="{prev["slug"]}.html">← {prev["title"]}</a>' if prev else '<a href="index.html">← Apresentação</a>')+(f'<a href="{nxt["slug"]}.html">{nxt["title"]} →</a>' if nxt else '<a href="index.html">Voltar ao início ↑</a>')+'</nav>'
 toc=''.join(f'<a href="#{a}">{b}</a>' for a,b in heads)
 (R/(c['slug']+'.html')).write_text(layout(c['title'],body,c['slug'],toc))
 text=html.unescape(re.sub('<[^>]+>',' ',c['body']));index.append({'title':c['title'],'url':c['slug']+'.html','text':re.sub(r'\s+',' ',text)})
hero='''<p class="eyebrow">UM LIVRO DO CURSO SDM 2026</p><h1>Da pergunta<br>ao artigo</h1><p class="lead">Uma equipe, uma espécie e as decisões que transformam registros em conhecimento.</p><p class="intro">Aprenda os fundamentos da modelagem de distribuição de espécies acompanhando uma história de pesquisa. Conceitos essenciais, exemplos de R e um caminho organizado pelo ODMAP.</p><a class="start" href="01-a-pergunta.html">Começar a leitura →</a><figure class="cover"><img src="assets/equipe.png" alt="Os quatro pesquisadores dos quadrinhos do curso reunidos à mesa, discutindo mapas e dados."><figcaption>Os personagens do curso acompanham a jornada. A história é didática.</figcaption></figure><div class="odmap" aria-label="As cinco etapas do ODMAP"><span>O<br><small>Visão geral</small></span><span>D<br><small>Dados</small></span><span>M<br><small>Modelo</small></span><span>A<br><small>Avaliação</small></span><span>P<br><small>Predição</small></span></div><h2>Leia, experimente, volte às decisões</h2><p>O caso principal acompanha <i>Zaprionus tuberculatus</i>, do planejamento à projeção climática. Um segundo caso, com <i>Amanoa cupatensis</i>, apresenta Ensemble of Small Models. Você pode ler em sequência ou procurar uma etapa no sumário.</p><p>Os trechos de código indicam sua origem e os objetos necessários. As ilustrações e os mapas vêm do curso; as figuras de resultados são exemplos já presentes nos slides. O livro não apresenta uma nova execução dos modelos.</p><h2>O percurso</h2>'''
hero+='<div class="chapter-grid">'+''.join(f'<a href="{c["slug"]}.html"><span>{i:02d} · {c["stage"]}</span><h3>{c["title"]}</h3><p>{c["lead"]}</p></a>' for i,c in enumerate(cs,1))+'</div><h2>Materiais de trabalho</h2><p><a href="materiais/registro-odmap.csv">Registro ODMAP</a> · <a href="materiais/registro-refinamentos.csv">Registro de refinamentos</a> · <a href="materiais/roteiro-artigo.md">Roteiro de artigo</a> · <a href="18-fontes-e-reproducao.html">Scripts e fontes</a></p><p class="source">A organização de leitura foi inspirada em <a href="https://analises-ecologicas.com/">Análises Ecológicas no R</a>. Esta adaptação tem texto, narrativa e projeto visual próprios.</p>'
(R/'index.html').write_text(layout('Da pergunta ao artigo',hero))
(R/'assets/search-index.js').write_text('window.BOOK_SEARCH='+json.dumps(index,ensure_ascii=False)+';')
print(f'{len(cs)+1} páginas geradas.')

# Browser-readable versions of all linked text materials.
import csv
from urllib.parse import unquote, urlsplit
linked = set()
for page in R.glob('*.html'):
    for href in re.findall(r'href="([^"]+)"', page.read_text()):
        url = urlsplit(html.unescape(href))
        if url.scheme or url.netloc:
            continue
        source = R / unquote(url.path)
        if source.is_file() and source.suffix.lower() in {'.md', '.csv', '.r', '.json'}:
            linked.add(source)
previews = {source.relative_to(R).as_posix(): source.with_suffix('.html').relative_to(R).as_posix() for source in linked}

def browser_links(document):
    document = re.sub(r'\sdownload(?:="[^"]*")?', '', document)
    def link(match):
        href = html.unescape(match.group(1))
        return 'href="' + esc(previews.get(unquote(href), href)) + '"'
    return re.sub(r'href="([^"]+)"', link, document)

def markdown_document(text):
    # The supplied writing template uses headings, paragraphs and ordered/unordered lists.
    result = []
    paragraph = []
    list_kind = None
    def flush():
        if paragraph:
            result.append('<p>' + esc(' '.join(paragraph)) + '</p>')
            paragraph.clear()
    def close_list():
        nonlocal list_kind
        if list_kind:
            result.append('</' + list_kind + '>')
            list_kind = None
    for line in text.splitlines():
        heading = re.match(r'^(#{1,6}) (.*)', line)
        item = re.match(r'^(?:([-*]) |\d+\. )(.*)', line)
        if heading:
            flush(); close_list()
            level = min(6, len(heading.group(1)) + 1)
            result.append(f'<h{level}>' + esc(heading.group(2)) + f'</h{level}>')
        elif item:
            flush()
            kind = 'ul' if item.group(1) else 'ol'
            if list_kind != kind:
                close_list(); result.append('<' + kind + '>'); list_kind = kind
            result.append('<li>' + esc(item.group(2)) + '</li>')
        elif not line.strip():
            flush(); close_list()
        else:
            close_list(); paragraph.append(line)
    flush(); close_list()
    return ''.join(result)

titles = {
    'roteiro-artigo': 'Roteiro de artigo',
    'registro-odmap': 'Registro de decisões ODMAP',
    'registro-refinamentos': 'Registro de refinamentos',
    'slides-texto': 'Texto extraído dos slides',
    'links-dos-slides': 'Links dos slides',
    'imagens': 'Origem das imagens',
    'manifesto': 'Manifesto das fontes',
}
for source in sorted(linked):
    title = titles.get(source.stem, source.stem)
    content = source.read_text()
    if source.suffix.lower() == '.md':
        # Use the material title as the page h1, without repeating its Markdown h1.
        content = re.sub(r'^# .*\n', '', content, count=1)
        body = markdown_document(content)
    elif source.suffix.lower() == '.csv':
        rows = list(csv.reader(content.splitlines()))
        body = '<div class="table-wrap"><table><thead><tr>' + ''.join('<th scope="col">'+esc(x)+'</th>' for x in rows[0]) + '</tr></thead><tbody>'
        body += ''.join('<tr>'+''.join('<td>'+esc(x or '—')+'</td>' for x in row)+'</tr>' for row in rows[1:])
        body += '</tbody></table></div>'
    else:
        language = 'R' if source.suffix.lower() == '.r' else 'JSON'
        body = '<div class="code-card"><div class="code-head"><span>'+language+' · conteúdo integral</span><button class="copy" type="button">Copiar</button></div><pre><code>'+esc(content)+'</code></pre></div>'
    body = '<p class="eyebrow">MATERIAL DE CONSULTA</p><h1>'+esc(title)+'</h1>' + body
    body += '<nav class="pager" aria-label="Voltar ao livro"><a href="index.html">← Início do livro</a><a href="18-fontes-e-reproducao.html">Scripts e fontes →</a></nav>'
    page = browser_links(layout(title, body))
    depth = len(source.relative_to(R).parts) - 1
    page = page.replace('<head>', '<head><base href="' + '../' * depth + '">', 1)
    page = page.replace('href="#main"', 'href="' + source.with_suffix('.html').relative_to(R).as_posix() + '#main"')
    source.with_suffix('.html').write_text(page)
for page in R.glob('*.html'):
    page.write_text(browser_links(page.read_text()))
print(f'{len(linked)} materiais preparados para leitura no navegador, sem download automático.')
