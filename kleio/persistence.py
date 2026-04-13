"""Property store for Kleio translation state.

Provides a simple key-value property system that replaces the Prolog
persistence.pl module's get_prop/set_prop/del_props predicates.
Each PropertyStore instance is independent (thread-safe by isolation).
"""
from __future__ import annotations
from typing import Any, Optional
import threading


class PropertyStore:
    """Thread-safe hierarchical property store.
    
    Stores properties as object -> property_name -> value mappings.
    Each translation context should use its own PropertyStore instance.
    """

    def __init__(self):
        """Initialize an empty property store."""
        self._store: dict[str, dict[str, Any]] = {}
        self._values: dict[str, Any] = {}  # Simple key-value store
        self._lock = threading.Lock()

    def put_value(self, key: str, value: Any) -> None:
        """Store a simple key-value pair.
        
        Args:
            key: The key to store under.
            value: The value to store.
        """
        with self._lock:
            self._values[key] = value

    def get_value(self, key: str, default: Any = None) -> Any:
        """Retrieve a simple value by key.
        
        Args:
            key: The key to look up.
            default: Default value if key not found.
        
        Returns:
            The stored value or default.
        """
        with self._lock:
            return self._values.get(key, default)

    def has_value(self, key: str) -> bool:
        """Check if a simple value exists.
        
        Args:
            key: The key to check.
        
        Returns:
            True if the key exists.
        """
        with self._lock:
            return key in self._values

    def set_prop(self, obj: str, prop: str, value: Any) -> None:
        """Set a property on an object.
        
        Args:
            obj: The object identifier.
            prop: The property name.
            value: The property value.
        """
        with self._lock:
            if obj not in self._store:
                self._store[obj] = {}
            self._store[obj][prop] = value

    def get_prop(self, obj: str, prop: str, default: Any = None) -> Any:
        """Get a property value from an object.
        
        Args:
            obj: The object identifier.
            prop: The property name.
            default: Default value if not found.
        
        Returns:
            The property value or default.
        """
        with self._lock:
            return self._store.get(obj, {}).get(prop, default)

    def has_prop(self, obj: str, prop: str) -> bool:
        """Check if an object has a property.
        
        Args:
            obj: The object identifier.
            prop: The property name.
        
        Returns:
            True if the property exists on the object.
        """
        with self._lock:
            return obj in self._store and prop in self._store[obj]

    def del_props(self, obj: str) -> None:
        """Delete all properties of an object.
        
        Args:
            obj: The object identifier.
        """
        with self._lock:
            self._store.pop(obj, None)

    def del_prop(self, obj: str, prop: str) -> None:
        """Delete a single property from an object.
        
        Args:
            obj: The object identifier.
            prop: The property name.
        """
        with self._lock:
            if obj in self._store:
                self._store[obj].pop(prop, None)

    def get_props(self, obj: str) -> dict[str, Any]:
        """Get all properties of an object as a dictionary.
        
        Args:
            obj: The object identifier.
        
        Returns:
            A copy of the object's properties.
        """
        with self._lock:
            return dict(self._store.get(obj, {}))

    def add_to_prop(self, obj: str, prop: str, value: Any) -> None:
        """Add a value to a list property (creates list if needed).
        
        Args:
            obj: The object identifier.
            prop: The property name.
            value: The value to add.
        """
        with self._lock:
            if obj not in self._store:
                self._store[obj] = {}
            current = self._store[obj].get(prop, [])
            if not isinstance(current, list):
                current = [current]
            current.append(value)
            self._store[obj][prop] = current

    def clear(self) -> None:
        """Clear all stored data."""
        with self._lock:
            self._store.clear()
            self._values.clear()
