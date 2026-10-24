"""Rebuild the bibliography from fontes/referencias.json without network access."""
from pathlib import Path
import json,re,html,unicodedata,hashlib
R=Path(__file__).resolve().parents[1]
def H(value):return html.escape(str(value or ''))
def clean(value):return html.unescape(re.sub('<[^>]*>','',str(value or ''))).strip()
def norm(value):return ''.join(c for c in unicodedata.normalize('NFD',clean(value).lower()) if not unicodedata.combining(c))
def authors(d):
 creators=d.get('creators') or []
 aa=[a for a in creators if a.get('creatorType') in [None,'author']]
 if not aa:aa=[a for a in creators if a.get('creatorType')=='editor']
 result=[]
 for a in aa:
  family=a.get('lastName') or a.get('name','');given=a.get('firstName') or ''
  initials=' '.join(x[0]+'.' for x in re.findall(r'[\wÀ-ÿ]+',given))
  result.append(family+(', '+initials if initials else ''))
 return '; '.join(result)
def key(d):return (d.get('DOI') or '').lower().strip().removeprefix('https://doi.org/') or norm(d['title'])
raw=json.loads((R/'fontes/referencias.json').read_text());refs={};resources=[]
for x in raw:
 if not x.get('data'):
  resources.append(x);continue
 k=key(x['data'])
 if k not in refs:refs[k]=dict(x)
 else:
  old=refs[k];old['slides']=sorted(set(old.get('slides',[])+x.get('slides',[])))
  if x.get('added_by_authorship'):old['added_by_authorship']=True
  keys=set(old.get('zotero_keys') or [])|set(x.get('zotero_keys') or [])
  if keys:old['zotero_keys']=sorted(keys)
body='<p>Referências dos slides do curso e artigos de Amaro identificados no Zotero em qualquer posição de autoria. As entradas estão em ordem alfabética; repetições foram reunidas pelo DOI ou pelo título. A origem de cada entrada é indicada abaixo da referência.</p><h2>Publicações e obras com dados bibliográficos</h2><ol class="references">'
for k,x in sorted(refs.items(),key=lambda t:norm(authors(t[1]['data'])+' '+t[1]['data']['title'])):
 d=x['data'];who=authors(d).rstrip('.');year=re.search(r'(?:18|19|20)\d\d',d.get('date') or '')
 txt=(H(who)+'. ' if who else '')+(year.group() if year else 's.d.')+'. <i>'+H(clean(d['title']).rstrip('.'))+'</i>.'
 pub=clean(d.get('publicationTitle') or d.get('bookTitle') or '')
 if pub:txt+=' '+H(pub)
 if d.get('volume'):txt+=', '+H(d['volume'])
 if d.get('issue'):txt+='('+H(d['issue'])+')'
 if d.get('pages'):txt+=': '+H(d['pages'])
 if pub:txt+='.'
 elif d.get('publisher'):txt+=' '+H(d['publisher'])+'.'
 doi=d.get('DOI') or '';url='https://doi.org/'+doi if doi else (d.get('url') or x.get('url') or '')
 if url:txt+=' <a href="'+H(url)+'">'+('DOI' if doi else 'Acessar fonte')+'</a>.'
 if doi and x.get('url') and x['url']!=url:txt+=' <a href="'+H(x['url'])+'">'+('Link no Zotero' if x.get('added_by_authorship') and not x.get('slides') else 'Link nos slides')+'</a>.'
 source=[]
 if x.get('slides'):source.append('Slides: '+', '.join(map(str,x['slides']))+'.')
 if x.get('added_by_authorship'):source.append('Zotero: artigo com Amaro entre os autores.')
 if not pub and not year:source.append('Registro sem periódico e data informados no Zotero.')
 txt+='<small class="ref-source">'+H(' '.join(source))+'</small>'
 rid='ref-'+hashlib.sha1(k.encode()).hexdigest()[:10];body+='<li id="'+rid+'">'+txt+'</li>'
body+='</ol><h2>Bases de dados, programas e recursos online</h2><ul class="references">'
for x in sorted(resources,key=lambda x:norm(max(x.get('labels') or [x['url']],key=len))):
 label=max(x.get('labels') or [x['url']],key=len)
 body+='<li>'+H(label)+'. <a href="'+H(x['url'])+'">Acessar recurso</a>.<small class="ref-source">Slides: '+', '.join(map(str,x.get('slides',[])))+'.</small></li>'
body+='</ul>'
p=R/'capitulos/19-referencias.json';chapter=json.loads(p.read_text());chapter.update(lead='Publicações dos slides, artigos com Amaro entre os autores e recursos do curso.',body=body);p.write_text(json.dumps(chapter,ensure_ascii=False,indent=2))
print(len(refs),'referências bibliográficas;',len(resources),'recursos online.')
