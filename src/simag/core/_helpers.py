# -*- coding: utf-8 -*-
"""Helper classes, functions and tools."""

#===================#
#      CLASSES      #
#===================#

from ast import literal_eval
from collections import MutableSet

import xml.etree.ElementTree as xml

class OrderedSet(MutableSet):
    """
    An OrderedSet is a custom MutableSet that remembers its order, so that every
    entry has an index that can be looked up.
    
    Based on `https://github.com/LuminosoInsight/ordered-set` and released under 
    MIT license.
    """
    
    SLICE_ALL = slice(None)
    def __init__(self, iterable=None):
        self.items = []
        self.map = {}
        if iterable is not None:
            self |= iterable

    def __len__(self):
        return len(self.items)

    def __getitem__(self, index):
        """
        Get the item at a given index.
        If `index` is a slice, you will get back that slice of items. If it's
        the slice [:], exactly the same object is returned. (If you want an
        independent copy of an OrderedSet, use `OrderedSet.copy()`.)
        If `index` is an iterable, you'll get the OrderedSet of items
        corresponding to those indices. This is similar to NumPy's
        "fancy indexing".
        """
        if index == self.SLICE_ALL:
            return self
        elif hasattr(index, '__index__') or isinstance(index, slice):
            result = self.items[index]
            if isinstance(result, list):
                return OrderedSet(result)
            else:
                return result
        elif self.is_iterable(index):
            return OrderedSet([self.items[i] for i in index])
        else:
            raise TypeError("Don't know how to index an OrderedSet by %r" %
                    index)

    def copy(self):
        return OrderedSet(self)

    def __getstate__(self):
        if len(self) == 0:
            # The state can't be an empty list.
            # We need to return a truthy value, or else __setstate__ won't be run.
            #
            # This could have been done more gracefully by always putting the state
            # in a tuple, but this way is backwards- and forwards- compatible with
            # previous versions of OrderedSet.
            return (None,)
        else:
            return list(self)

    def __setstate__(self, state):
        if state == (None,):
            self.__init__([])
        else:
            self.__init__(state)

    def __contains__(self, key):
        return key in self.map

    def add(self, key):
        """
        Add `key` as an item to this OrderedSet, then return its index.
        If `key` is already in the OrderedSet, return the index it already
        had.
        """
        if key not in self.map:
            self.map[key] = len(self.items)
            self.items.append(key)
        return self.map[key]
    append = add

    def update(self, sequence):
        """
        Update the set with the given iterable sequence, then return the index
        of the last element inserted.
        """
        item_index = None
        try:
            for item in sequence:
                item_index = self.add(item)
        except TypeError:
            raise ValueError('Argument needs to be an iterable, got %s' % type(sequence))
        return item_index

    def index(self, key):
        """
        Get the index of a given entry, raising an IndexError if it's not
        present.
        `key` can be an iterable of entries that is not a string, in which case
        this returns a list of indices.
        """
        if self.is_iterable(key):
            return [self.index(subkey) for subkey in key]
        return self.map[key]

    def pop(self):
        """
        Remove and return the last element from the set.
        
        Raises KeyError if the set is empty.
        """
        if not self.items:
            raise KeyError('Set is empty')

        elem = self.items[-1]
        del self.items[-1]
        del self.map[elem]
        return elem

    def discard(self, key):
        """
        Remove an element.  Do not raise an exception if absent.
        The MutableSet mixin uses this to implement the .remove() method, which
        *does* raise an error when asked to remove a non-existent item.
        """
        if key in self:
            i = self.items.index(key)
            del self.items[i]
            del self.map[key]
            for k, v in self.map.items():
                if v >= i:
                    self.map[k] = v - 1

    def clear(self):
        """
        Remove all items from this OrderedSet.
        """
        del self.items[:]
        self.map.clear()

    def __iter__(self):
        return iter(self.items)

    def __reversed__(self):
        return reversed(self.items)

    def __repr__(self):
        if not self:
            return '%s()' % (self.__class__.__name__,)
        return '%s(%r)' % (self.__class__.__name__, list(self))

    def __eq__(self, other):
        if isinstance(other, OrderedSet):
            return len(self) == len(other) and self.items == other.items
        try:
            other_as_set = set(other)
        except TypeError:
            # If `other` can't be converted into a set, it's not equal.
            return False
        else:
            return set(self) == other_as_set
    
    @staticmethod
    def is_iterable(obj):
        """
        Are we being asked to look up a list of things, instead of a single thing?
        We check for the `__iter__` attribute so that this can cover types that
        don't have to be known by this module, such as NumPy arrays.
        Strings, however, should be considered as atomic values to look up, not
        iterables. The same goes for tuples, since they are immutable and therefore
        valid entries. 
        We don't need to check for the Python 2 `unicode` type, because it doesn't
        have an `__iter__` attribute anyway.
        """
        return hasattr(obj, '__iter__') and not isinstance(obj, str) and not isinstance(obj, tuple)
    
    @classmethod
    @property
    def SLICE_ALL(cls):
        return cls.SLICE_ALL

#==================#
#       TOOLS      #
#==================#


def import_configs(cfg_file):

    def iter_balsheet():
        entries = {}
        for item in node:
            try:
                val = item.attrib['val']
            except:
                val = None
            quant = item.attrib['quant']
            entries[item.tag] = (quant, val)
        return entries

    data = xml.parse(cfg_file)
    root = data.getroot()
    cfg_type = root.attrib['cfg']
    if cfg_type == 'agents':
        agents = {}
        configs = {}
        for agent in root:
            ag_type = agent.attrib['type']
            ag_id = agent.attrib['id']
            if ag_type == 'instance':
                agents[ag_id] = {}
                for node in agent:
                    if node.tag == 'properties':
                        agents[ag_id]['properties'] = node.attrib
                    elif node.tag == 'position':
                        agents[ag_id]['position'] = literal_eval(node.text)
                    elif node.tag == 'assets':
                        agents[ag_id]['assets'] = iter_balsheet()
                    elif node.tag == 'liabilities':
                        agents[ag_id]['liabilities'] = iter_balsheet()
                    elif node.tag == 'percept_modes':
                        modes = node.text.split(',')
                        modes = [x.strip() for x in modes]
                        agents[ag_id]['properties']['percept_modes'] = modes
                    elif node.tag == 'eval_modes':
                        modes = node.text.split(',')
                        modes = [x.strip() for x in modes]
                        agents[ag_id]['properties']['eval_modes'] = modes
                    elif node.tag == 'logic':
                        formulae = []
                        for item in node:
                            formulae.append(item)
                        agents[ag_id]['logic'] = formulae
            else:
                assets, liabilities = {}, {}
                for node in agent:
                    if node.tag == 'properties':
                        props = node.attrib
                    elif node.tag == 'assets':
                        assets = iter_balsheet(node)
                    elif node.tag == 'liabilities':
                        liabilities = iter_balsheet(node)
                    elif node.tag == 'logic':
                        formulae = []
                        for item in node:
                            formulae.append(item)
                configs[ag_id] = [props, assets, liabilities, formulae]
        return agents, configs

