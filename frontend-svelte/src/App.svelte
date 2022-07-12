<script lang="ts">
  import {onMount} from 'svelte';
  import axios from 'axios';
  import Fuse from 'fuse.js';
  import ProductItem from './lib/ProductItem.svelte';
  import {isEmpty, last} from 'lodash';

  let products: any = [];
  let selectedProducts: any[] = [];

  const selectProduct = (product: any) => {
    if (selectedProducts.includes(product)) return;
    selectedProducts = selectedProducts.concat(product);
  };

  const removeProduct = (product: any) => {
    console.log('>>>', product, selectedProducts);
    selectedProducts = selectedProducts.filter(
      p => p.detail.link !== product.detail.link
    );
  };

  let fuse: Fuse<any> | null = null;

  const lastList = (prices: {list: number; special: number}[]) =>
    last(prices)?.list || 0;
  const firstSpecial = (prices: {list: number; special: number}[]) =>
    prices[0]?.special || 0;
  const calculateReduction = (aPrice: number, bPrice: number) =>
    (1 - aPrice / bPrice) * 100;

  onMount(async () => {
    const {data} = await axios.get('http://localhost:3000/products');
    products = data.map(d => {
      const prices = d.prices.sort(
        (a, b) => new Date(b.date).getTime() - new Date(a.date).getTime()
      );
      const reduction = calculateReduction(
        firstSpecial(prices),
        lastList(prices)
      );
      const brandTags = d.detail.name.split(' ').filter(x => {
        return ['apple', 'samsung'].find(i => x.toLowerCase().includes(i));
      });
      const modelTags = d.detail.name.split(' ').filter(x => {
        return ['galaxy', 'watch', 'macbook', 'pro', 'ultra'].find(i =>
          x.toLowerCase().includes(i)
        );
      });
      const numberTags = d.detail.name.split(' ').filter(x => /[0-9]/.test(x));
      // console.log('>>>', brandTags, numberTags);
      return {
        ...d,
        prices,
        numberTags,
        brandTags,
        modelTags,
        reduction,
      };
    });

    // Create the Fuse index
    const options = {
      keys: ['numberTags', 'brandTags', 'detail.name'],
      includeScore: true,
      threshold: 0.8,
    };
    const myIndex = Fuse.createIndex(options.keys, products);
    fuse = new Fuse(products, options, myIndex);
  });

  let searchInput: string = 'galaxy watch 46mm';
  let searchResult = [];

  $: productList =
    !searchInput || isEmpty(searchResult) ? products : searchResult;

  const doSearch = () => {
    searchResult = (fuse?.search(searchInput) || []).map(x => ({
      ...x.item,
      score: x.score,
    }));
  };

  const clearSearch = () => {
    searchInput = '';
    searchResult = [];
  };
</script>

<main>
  <div class="drawer drawer-end">
    <input id="my-drawer-4" type="checkbox" class="drawer-toggle" />
    <div class="drawer-content">
      <label
        for="my-drawer-4"
        class="drawer-button btn btn-primary fixed bottom-0 right-0"
      >
        Selected Items
      </label>
      <div class="px-5 py-5">
        <form class="flex gap-3 px-2 py-2" on:submit|preventDefault={doSearch}>
          <input
            bind:value={searchInput}
            class="input input-bordered input-primary"
          />
          <button on:click={doSearch} type="submit" class="btn">Search</button>
          <button on:click={clearSearch} type="button" class="btn">Clear</button
          >
        </form>
        <div class="grid grid-cols-3 gap-5">
          {#each productList as product}
            <div class="flex flex-col justify-center">
              <ProductItem {product} />
              <div class="flex justify-center my-3">
                {#if !selectedProducts.includes(product)}
                  <button
                    on:click={() => selectProduct(product)}
                    class="btn btn-sm btn-primary"
                  >
                    Select
                  </button>
                {:else}
                  <button
                    on:click={() => removeProduct(product)}
                    class="btn btn-sm btn-accent"
                  >
                    Remove
                  </button>
                {/if}
              </div>
            </div>
          {/each}
        </div>
      </div>
    </div>
    <div class="drawer-side">
      <label for="my-drawer-4" class="drawer-overlay" />
      <div
        class="flex flex-col p-4 overflow-y-auto w-80 bg-base-100 text-base-content"
      >
        {#each selectedProducts as product}
          <div>
            <ProductItem {product} />
            <button on:click={() => removeProduct(product)} class="btn btn-xs">
              Remove
            </button>
          </div>
        {/each}
      </div>
    </div>
  </div>
</main>

<style>
</style>
