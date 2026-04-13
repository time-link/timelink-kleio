"""Abstract base class for Kleio exporters."""
from __future__ import annotations
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional

from kleio.parser.models import ParsedGroup
from kleio.schema.registry import SchemaRegistry
from kleio.inference.models import InferenceResults


class Exporter(ABC):
    """Base class for Kleio data exporters.
    
    Exporters receive completed groups from the parser and produce
    output in various formats (XML, JSON, etc.).
    """
    
    @abstractmethod
    def init(self, source_file: str, output_dir: Path, 
             schema: SchemaRegistry, **kwargs) -> None:
        """Initialize the exporter for a new translation.
        
        Args:
            source_file: Path to the source .cli file being translated
            output_dir: Directory for output files
            schema: The loaded structure schema
        """
        ...
    
    @abstractmethod
    def export_group(self, group: ParsedGroup) -> None:
        """Export a single completed group.
        
        Called for each group as it is completed by the parser.
        """
        ...
    
    @abstractmethod
    def close(self, inference_results: Optional[InferenceResults] = None) -> list[str]:
        """Finalize the export.
        
        Called after all groups have been processed.
        
        Args:
            inference_results: Optional inference results to include in output
            
        Returns:
            List of output file paths created
        """
        ...
