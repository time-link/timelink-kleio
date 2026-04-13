"""Linked data resolution for Kleio.

Resolves external identifiers like @wikidata:Qxxxxxx annotations
to their labels and metadata. Caches results to avoid repeated lookups.
"""
from __future__ import annotations
from dataclasses import dataclass, field
from typing import Optional
import logging

logger = logging.getLogger(__name__)


@dataclass
class LinkedDataEntry:
    """A resolved linked data entry."""
    source: str         # e.g., "wikidata"
    identifier: str     # e.g., "Q12345"
    label: str = ""
    description: str = ""
    url: str = ""


class LinkedDataResolver:
    """Resolves and caches linked data references.
    
    Supports annotations in the format @source:identifier, e.g.:
    - @wikidata:Q12345
    - @geonames:123456
    
    Results are cached to avoid repeated network lookups.
    """

    def __init__(self):
        """Initialize the resolver with an empty cache."""
        self._cache: dict[str, LinkedDataEntry] = {}

    def resolve(self, annotation: str) -> Optional[LinkedDataEntry]:
        """Resolve a linked data annotation like '@wikidata:Q12345'.
        
        Returns cached result if available, otherwise attempts resolution.
        
        Args:
            annotation: The annotation string to resolve.
        
        Returns:
            A LinkedDataEntry if resolved, None if invalid format.
        """
        if annotation in self._cache:
            return self._cache[annotation]

        entry = self._parse_and_resolve(annotation)
        if entry:
            self._cache[annotation] = entry
        return entry

    def _parse_and_resolve(self, annotation: str) -> Optional[LinkedDataEntry]:
        """Parse annotation format and resolve.
        
        Args:
            annotation: The annotation string to parse.
        
        Returns:
            A LinkedDataEntry if valid format, None otherwise.
        """
        if not annotation.startswith("@"):
            return None

        parts = annotation[1:].split(":", 1)
        if len(parts) != 2:
            return None

        source, identifier = parts[0].lower(), parts[1]

        if source == "wikidata":
            return self._resolve_wikidata(identifier)

        logger.warning(f"Unknown linked data source: {source}")
        return LinkedDataEntry(
            source=source,
            identifier=identifier,
            url=f"https://{source}.org/wiki/{identifier}"
        )

    def _resolve_wikidata(self, qid: str) -> LinkedDataEntry:
        """Resolve a Wikidata entity.
        
        Currently returns a stub with URL. Full implementation would
        use httpx to call the Wikidata API for labels and descriptions.
        
        Args:
            qid: The Wikidata entity ID (e.g., Q12345).
        
        Returns:
            A LinkedDataEntry with Wikidata information.
        """
        # Full implementation would use httpx to call Wikidata API
        # For now, return a stub with the URL
        return LinkedDataEntry(
            source="wikidata",
            identifier=qid,
            url=f"https://www.wikidata.org/wiki/{qid}",
            label=qid,  # Placeholder until API integration
        )

    def cache_entry(self, annotation: str, entry: LinkedDataEntry) -> None:
        """Manually cache an entry.
        
        Useful for pre-populating the cache with known values.
        
        Args:
            annotation: The annotation string as key.
            entry: The LinkedDataEntry to cache.
        """
        self._cache[annotation] = entry

    def clear_cache(self) -> None:
        """Clear the resolution cache."""
        self._cache.clear()

    def get_cached(self, annotation: str) -> Optional[LinkedDataEntry]:
        """Get a cached entry without attempting resolution.
        
        Args:
            annotation: The annotation string to look up.
        
        Returns:
            The cached entry, or None if not in cache.
        """
        return self._cache.get(annotation)

    def has_cached(self, annotation: str) -> bool:
        """Check if an annotation is cached.
        
        Args:
            annotation: The annotation string to check.
        
        Returns:
            True if the annotation is in the cache.
        """
        return annotation in self._cache
