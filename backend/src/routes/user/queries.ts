export const userPopularityScoreByPlaylistIdQuery = (_id: string) => `call{
  match (u:User{_id: "${_id}"})-[:CREATED]->(p:Playlist)<-[l:LIKES]-(tizio)
  return p, 0.5*count(l) as score
  union
  match (u:User{_id: "${_id}"})-[:CREATED]->(p:Playlist)<-[c:CLONES]-(p2)
  return p, count(c) as score
}
return p, sum(score) as popularityScore`;

export const userPopularityScoreQuery = (_id: string) => `call{
  match (u:User{_id: "${_id}"})-[:CREATED]->(p:Playlist)<-[l:LIKES]-(tizio)
  return p, 0.5*count(l) as score
  union
  match (u:User{_id: "${_id}"})-[:CREATED]->(p:Playlist)<-[c:CLONES]-(p2)
  return p, count(c) as score
}
return sum(score) as popularityScore`;

export const bestContributorToUserPopularityScoreQuery = (_id: string) => `call{
  match (u:User{_id: "${_id}"})-[:CREATED]->(p:Playlist)<-[l:LIKES]-(tizio)
  return tizio, 0.5*count(l) as score
  union 
  match (u:User{_id: "${_id}"})-[:CREATED]->(p:Playlist)<-[c:CLONES]-(p2)<-[:CREATED]-(tizio)
  return tizio, count(c) as score
}
return tizio, sum(score) as contributeScore
order by contributeScore desc
limit 1`;

export const userPopularityScoreByOwnPlaylist = `call{
  match (u:User)-[:CREATED]->(p:Playlist)<-[l:LIKES]-(tizio)
  return u, p, 0.5*count(l) as score
  union
  match (u:User)-[:CREATED]->(p:Playlist)<-[c:CLONES]-(p2)
  return u, p, count(c) as score
}
return u, sum(score) as score
ORDER BY score DESC LIMIT 20`;
