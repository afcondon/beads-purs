export const isInfixOf = needle => haystack =>
  haystack.toLowerCase().includes(needle.toLowerCase());
