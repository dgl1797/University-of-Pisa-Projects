export const playlistsRanks = `call{
  match (u:User)-[:CREATED]->(p:Playlist)<-[l:LIKES]-(tizio)
  return p, 0.5*count(l) as score
  union
  match (u:User)-[:CREATED]->(p:Playlist)<-[c:CLONES]-(p2)
  return p, count(c) as score
}
return p, sum(score) as score
ORDER BY score DESC LIMIT 20`;

export const sortedPaginationByLikesCount = (
  shouldNotPaginate: boolean,
  ordering: -1 | 1 | undefined,
  filtering: {
    active: boolean;
    userId: string;
  },
  page: number,
  size: number
) =>
  `match (p:Playlist)<-[:LIKES]-(user${
    filtering?.active ? `:User{_id: "${filtering.userId}"}` : ""
  }) with distinct p match (p)<-[l:LIKES]-(anyone) 
  return p, count(l) as likes ${
    !!ordering ? `order by likes ${ordering === 1 ? "ASC" : "DESC"}` : ""
  } ${shouldNotPaginate ? "" : `SKIP ${page * size} LIMIT ${size}`}`;

export const likeSuggestion = (
  userId: string,
  sort: "ASC" | "DESC" | undefined,
  pageNumber: number,
  pageSize: number
) => `match (me:User{_id: "${userId}"})-[:LIKES]->(lp:Playlist)
with me, lp match (lp)<-[:LIKES]-(u2:User) where u2._id<>me._id
with distinct u2, lp match (u2)-[:LIKES]->(suggested:Playlist) where suggested._id<>lp._id
with distinct suggested match (suggested)<-[l:LIKES]-(anyone)
return distinct suggested, count(l) as likes ${
  sort !== undefined ? `order by likes ${sort}` : ""
} SKIP ${pageNumber * pageSize} LIMIT ${pageSize}`;

export const hopsSuggestion = (
  userId: string,
  sort: "ASC" | "DESC" | undefined,
  pageNumber: number,
  pageSize: number
) => `match (me:User{_id: "${userId}"})-[:CREATED]->(p:Playlist)-[:CLONES]->(f:Playlist)
with me, f match (f)-[:CLONES*]->(suggested:Playlist) where suggested._id<>f._id
with distinct suggested
match (suggested)<-[l:LIKES]-(anyone)
return distinct suggested, count(l) as likes ${
  sort !== undefined ? `order by likes ${sort}` : ""
} SKIP ${pageNumber * pageSize} LIMIT ${pageSize}`;

export const playlistPopularityScore = (pid: string) => `call{
  match (p:Playlist{_id: "${pid}"})<-[l:LIKES]-(anyone)
  return p, 0.5*count(l) as score
  union
  match (p:Playlist{_id: "${pid}"})<-[c:CLONES]-(anything)
  return p, count(c) as score
}
return p, sum(score) as popScore`;
